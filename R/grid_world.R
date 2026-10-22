# =============================================================================
# grid_world.R -- Grid-World Environment for Robot Navigation
# =============================================================================
# Responsibilities:
#   - Generate 2D grid maps with configurable obstacle density
#   - Place start and goal positions (ensuring reachability)
#   - Enumerate valid neighbours (4- or 8-directional movement)
#   - Compute edge costs (unit for cardinal, sqrt(2) for diagonal)
#   - Detect disconnected / unreachable goals via BFS flood-fill
#   - Visualise the grid with ggplot2
# =============================================================================

library(ggplot2)

CELL_FREE     <- 0L
CELL_OBSTACLE <- 1L
CELL_START    <- 2L
CELL_GOAL     <- 3L

MOVE_4 <- "four"
MOVE_8 <- "eight"

#' Create a grid-world map.
create_grid <- function(width        = 20L,
                        height       = 20L,
                        obs_density  = 0.20,
                        movement     = MOVE_4,
                        seed         = NULL,
                        max_attempts = 50L) {
  stopifnot(is.numeric(width)  && width  >= 3L)
  stopifnot(is.numeric(height) && height >= 3L)
  stopifnot(is.numeric(obs_density) && obs_density >= 0 && obs_density < 0.9)
  stopifnot(movement %in% c(MOVE_4, MOVE_8))

  width  <- as.integer(width)
  height <- as.integer(height)

  if (!is.null(seed)) set.seed(seed)
  stored_seed <- if (!is.null(seed)) as.integer(seed) else NA_integer_

  n_cells <- width * height
  n_obs   <- floor(obs_density * n_cells)

  grid <- matrix(CELL_FREE, nrow = height, ncol = width)
  start <- c(1L, 1L)
  goal  <- c(height, width)

  for (attempt in seq_len(max_attempts)) {
    free_idx <- seq_len(n_cells)
    obs_idx  <- sample(free_idx, min(n_obs, length(free_idx) - 2L))
    grid <- matrix(CELL_FREE, nrow = height, ncol = width)
    for (idx in obs_idx) {
      r <- ((idx - 1L) %/% width) + 1L
      c <- ((idx - 1L) %% width)  + 1L
      grid[r, c] <- CELL_OBSTACLE
    }

    free_cells <- which(grid == CELL_FREE, arr.ind = TRUE)
    if (nrow(free_cells) < 2L) next

    idx_s <- sample(nrow(free_cells), 1L)
    start <- free_cells[idx_s, ]

    remaining <- free_cells[-idx_s, , drop = FALSE]
    if (nrow(remaining) == 0L) next
    start <- c(row = as.integer(start[1]), col = as.integer(start[2]))
    goal  <- c(row = as.integer(goal[1]),  col = as.integer(goal[2]))

    reachable <- .bfs_reachable(grid, start, goal, movement)
    if (reachable) {
      return(list(grid=grid, start=start, goal=goal,
                  width=width, height=height, movement=movement,
                  obs_density=obs_density, seed=stored_seed, reachable=TRUE))
    }
  }

  warning(sprintf("Could not find connected map after %d attempts (density=%.2f).", max_attempts, obs_density))
  start <- c(row = as.integer(start[1]), col = as.integer(start[2]))
  goal  <- c(row = as.integer(goal[1]),  col = as.integer(goal[2]))
  list(grid=grid, start=start, goal=goal,
       width=width, height=height, movement=movement,
       obs_density=obs_density, seed=stored_seed, reachable=FALSE)
}

#' Return valid neighbours of a cell.
get_neighbours <- function(grid, row, col, movement = MOVE_4) {
  height <- nrow(grid); width <- ncol(grid)
  dirs <- list(
    list(dr=-1L,dc= 0L,diag=FALSE), list(dr= 1L,dc= 0L,diag=FALSE),
    list(dr= 0L,dc=-1L,diag=FALSE), list(dr= 0L,dc= 1L,diag=FALSE))
  if (movement == MOVE_8) {
    dirs <- c(dirs, list(
      list(dr=-1L,dc=-1L,diag=TRUE), list(dr=-1L,dc= 1L,diag=TRUE),
      list(dr= 1L,dc=-1L,diag=TRUE), list(dr= 1L,dc= 1L,diag=TRUE)))
  }
  neighbours <- list()
  for (d in dirs) {
    nr <- row + d$dr; nc <- col + d$dc
    if (nr<1L || nr>height || nc<1L || nc>width) next
    if (grid[nr,nc] == CELL_OBSTACLE) next
    if (d$diag) { if(grid[row,nc]==CELL_OBSTACLE || grid[nr,col]==CELL_OBSTACLE) next }
    cost <- if (d$diag) sqrt(2) else 1.0
    neighbours <- c(neighbours, list(list(row=nr,col=nc,cost=cost,diagonal=d$diag)))
  }
  neighbours
}

cell_id <- function(row, col, width) (row - 1L) * width + col
id_to_cell <- function(id, width) c(row = as.integer(((id - 1L) %/% width) + 1L),
                                    col = as.integer(((id - 1L) %% width) + 1L))

#' Admissible heuristic for A*.
heuristic <- function(r1, c1, r2, c2, movement=MOVE_4, diag_cost=sqrt(2)) {
  dr <- abs(r1-r2); dc <- abs(c1-c2)
  if (movement == MOVE_4) return(dr + dc)
  diagonal <- min(dr, dc); straight <- abs(dr - dc)
  diagonal * diag_cost + straight * 1.0
}

.bfs_reachable <- function(grid, start, goal, movement) {
  height <- nrow(grid); width <- ncol(grid)
  visited <- matrix(FALSE, nrow=height, ncol=width)
  queue <- list(start); visited[start[1], start[2]] <- TRUE
  while (length(queue) > 0L) {
    cur <- queue[[1L]]; queue <- queue[-1L]
    if (cur[1]==goal[1] && cur[2]==goal[2]) return(TRUE)
    for (nb in get_neighbours(grid, cur[1], cur[2], movement)) {
      if (!visited[nb$row, nb$col]) {
        visited[nb$row, nb$col] <- TRUE
        queue <- c(queue, list(c(nb$row, nb$col)))
      }
    }
  }
  FALSE
}

#' Visualise a grid world with optional paths.
plot_grid <- function(grid_obj, paths=list(), title="Grid World", show_start_goal=TRUE) {
  g <- grid_obj$grid; height <- grid_obj$height; width <- grid_obj$width
  cell_df <- expand.grid(row=1:height, col=1:width)
  cell_df$type <- ifelse(g[cbind(cell_df$row, cell_df$col)]==CELL_OBSTACLE, "Obstacle","Free")
  p <- ggplot() +
    geom_tile(data=cell_df, aes(x=col, y=height-row+1, fill=type), colour="grey85", linewidth=0.2) +
    scale_fill_manual(name="Cell", values=c("Free"="#F5F5F5","Obstacle"="#2C2C54"), guide=guide_legend(order=1))
  path_colours <- c("#E63946","#06D6A0","#FFB703","#8338EC","#3A86FF")
  if (length(paths) > 0) {
    col_vals <- setNames(path_colours[seq_along(paths)], names(paths))
    for (i in seq_along(paths)) {
      path <- paths[[i]]; label <- names(paths)[i]
      if (length(path)==0) next
      path_df <- do.call(rbind, lapply(path, function(s) data.frame(col=s[2], row=height-s[1]+1, label=label)))
      p <- p +
        geom_path(data=path_df, aes(x=col,y=row,colour=label,group=label), linewidth=1.4, lineend="round") +
        geom_point(data=path_df, aes(x=col,y=row,colour=label), size=1.2, alpha=0.7)
    }
    p <- p + scale_colour_manual(name="Algorithm", values=col_vals)
  }
  if (show_start_goal) {
    sdf <- data.frame(col=grid_obj$start[2], row=height-grid_obj$start[1]+1)
    gdf <- data.frame(col=grid_obj$goal[2],  row=height-grid_obj$goal[1]+1)
    p <- p +
      geom_point(data=sdf, aes(x=col,y=row), shape=21, size=5, fill="#06D6A0", colour="white", stroke=1.5, inherit.aes=FALSE) +
      geom_text(data=sdf, aes(x=col,y=row,label="S"), colour="white", fontface="bold", size=3, inherit.aes=FALSE) +
      geom_point(data=gdf, aes(x=col,y=row), shape=21, size=5, fill="#E63946", colour="white", stroke=1.5, inherit.aes=FALSE) +
      geom_text(data=gdf, aes(x=col,y=row,label="G"), colour="white", fontface="bold", size=3, inherit.aes=FALSE)
  }
  p + scale_x_continuous(expand=expansion(add=0.5)) +
    scale_y_continuous(expand=expansion(add=0.5)) +
    coord_equal() +
    labs(title=title,
         subtitle=sprintf("%dx%d | Density: %.0f%% | Movement: %s", width, height, grid_obj$obs_density*100, grid_obj$movement),
         x="Column", y="Row") +
    theme_minimal(base_size=12) +
    theme(plot.title=element_text(face="bold",size=14,hjust=0.5),
          plot.subtitle=element_text(size=10,hjust=0.5,colour="grey50"),
          legend.position="right", panel.grid=element_blank())
}

generate_multiple_maps <- function(n=10L, base_seed=42L, ...) {
  lapply(seq_len(n), function(i) create_grid(seed=base_seed+i, ...))
}

grid_summary <- function(grid_obj) {
  data.frame(width=grid_obj$width, height=grid_obj$height,
             n_cells=grid_obj$width*grid_obj$height,
             n_obstacles=sum(grid_obj$grid==CELL_OBSTACLE),
             obs_density=grid_obj$obs_density, movement=grid_obj$movement,
             reachable=grid_obj$reachable, seed=grid_obj$seed,
             stringsAsFactors=FALSE)
}
