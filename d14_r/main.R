# You can find a list of CRAN mirrors at https://cran.r-project.org/mirrors.html.
options(repos = "https://cloud.r-project.org/")
# Install the animation package if not already installed
if (!require(animation)) {
  install.packages("magick")
}

# Load the animation package
library(magick)

main <- function(fn) {
  read_file_lines <- function(args) {
    if (length(args) == 0) {
      print("Usage: Rscript main.R <input-path>")
      return(NULL)
    }
    file_path <- args[1]
    if (!file.exists(file_path)) {
      print(paste("File", file_path, "does not exist."))
      return(NULL)
    }
    return(readLines(file_path))
  }

  parse_paths <- function(path_lines) {
    paths <- list()
    for (line in path_lines) {
      # Split the line into individual points
      path_points <- strsplit(line, "->")[[1]]
      # Create an empty matrix to store the path coordinates
      path_matrix <-
        matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c("x", "y")))
      for (point in path_points) {
        # Split each point into x and y coordinates
        xy <- strsplit(point, ",")[[1]]
        x <- xy[1]
        y <- xy[2]
        # Add the coordinates to the path matrix
        path_matrix <- rbind(path_matrix, c(as.integer(x), as.integer(y)))
      }
      # Add the path matrix to the list of paths
      paths <- append(paths, list(path_matrix))
    }
    return(paths)
  }

  create_line <- function(x1, y1, x2, y2, matrix) {
    if (x1 == x2) {
      # Vertical line
      y_seq <- as.character(min(y1, y2):max(y1, y2))
      matrix[y_seq, as.character(x1)] <- "#"
    } else if (y1 == y2) {
      # Horizontal line
      x_seq <- as.character(min(x1, x2):max(x1, x2))
      matrix[as.character(y1), x_seq] <- "#"
    }
    return(matrix)
  }

  create_path_matrix <- function(path_list) {
    max_y <- max(sapply(path_list, function(path) max(path[, "y"])))
    max_x <- max(sapply(path_list, function(path) max(path[, "x"])))
    min_x <- min(sapply(path_list, function(path) min(path[, "x"])))
    x_size <- max_x - min_x

    # Create an empty matrix with the determined dimensions
    path_matrix <- matrix(".", nrow = max_y + 1, ncol = x_size + 1)
    col_names <- min_x:max_x
    row_names <- 0:max_y
    dimnames(path_matrix) <- list(row_names, col_names)

    # Populate the matrix with lines from paths
    for (path in path_list) {
      prev_x <- path[1, "x"]
      prev_y <- path[1, "y"]
      for (i in seq_len(nrow(path))) {
        x <- path[i, "x"]
        y <- path[i, "y"]
        path_matrix <- create_line(prev_x, prev_y, x, y, path_matrix)
        prev_x <- x
        prev_y <- y
      }
    }

    return(path_matrix)
  }

  args <- commandArgs(trailingOnly = TRUE)
  lines <- read_file_lines(args)
  paths <- parse_paths(lines)
  path_matrix <- create_path_matrix(paths)

  build_path <- "build"
  # Create the build folder if it doesn't exist
  if (!file.exists(build_path)) {
    dir.create(build_path)
  }
  # Move into the build folder
  setwd(build_path)

  # execute main part
  animation <- fn(path_matrix)

  # Move into the parent folder
  setwd("..")
  image_write(animation, "result.gif")
  unlink(build_path, recursive = TRUE)
}

Visualizer <- function() {
  # Global counter variable
  counter <- 1
  # Function to generate a new filename with counter
  generate_filename <- function(extension) {
    filename <- paste0("image_", sprintf("%03d", counter), ".", extension)
    counter <<- counter + 1
    return(filename)
  }

  return(function(path_matrix, imgs) {
    nrow <- nrow(path_matrix)
    ncol <- ncol(path_matrix)
    # Generate the filename with counter
    filename <- generate_filename("png")
    # Save the plot as a PNG image
    png(filename)
    # Set up the plot
    plot(0, 0,
      type = "n",
      xlim = c(0, ncol), ylim = c(0, nrow),
      xlab = "x", ylab = "y"
    )

    # Add the lines to the plot
    for (i in 1:nrow) {
      for (j in 1:ncol) {
        switch(path_matrix[i, j],
          "#" = {
            if (i < nrow && path_matrix[i + 1, j] == "#") {
              # Vertical line
              lines(c(j, j), c(nrow - i + 1, nrow - i), col = "black")
            }
            # Check if the cell to the right is also a line to avoid duplicate
            if (j < ncol && path_matrix[i, j + 1] == "#") {
              # Horizontal line
              lines(c(j, j + 1), c(nrow - i + 1, nrow - i + 1), col = "black")
            }
          },
          "+" = {
            # Draw a point
            points(j, nrow - i + 1, col = "red", pch = "+", cex = 1)
          },
          "o" = {
            # Draw a point
            points(j, nrow - i + 1, col = "blue", pch = "o", cex = 1)
          }
        )
      }
    }
    dev.off()
    image <- image_read(filename)
    return(append(imgs, image))
  })
}


main(function(path_matrix) {
  imgs <- c()
  visualize_path_matrix <- Visualizer()
  imgs <- visualize_path_matrix(path_matrix, imgs)
  path_matrix["0", "500"] <- "+"
  imgs <- visualize_path_matrix(path_matrix, imgs)
  print(path_matrix)
  # Get the names of the first and last column
  first_col <- as.integer(colnames(path_matrix)[1])
  last_col <- as.integer(colnames(path_matrix)[ncol(path_matrix)])
  last_row <- as.integer(rownames(path_matrix)[nrow(path_matrix)])
  cur_position <- c("0", "500")
  while (TRUE) {
    if (cur_position[1] == last_row + 1 || cur_position[2] == first_col || cur_position[2] == last_col) {
      break # Exit the loop when sand reaches the bottom
    }
    # Check the cell below the current position
    cur_row <- as.integer(cur_position[1])
    cur_col <- as.integer(cur_position[2])

    if (cur_row == last_row) {
      break
    }
    if (cur_col == first_col || cur_col == last_col) {
      # Cell below is out of bounds, so back to source
      cur_position <- c("0", "500")
    }
    # Check the cell below the current position
    cell_below <- path_matrix[as.character(as.integer(cur_position[1]) + 1), cur_position[2]]
    cell_below_l <- path_matrix[as.character(as.integer(cur_position[1]) + 1), as.character(as.integer(cur_position[2]) - 1)]
    cell_below_r <- path_matrix[as.character(as.integer(cur_position[1]) + 1), as.character(as.integer(cur_position[2]) + 1)]
    if (cell_below == ".") {
      # Move the sand down by updating the current position
      path_matrix[cur_position[1], cur_position[2]] <- "."
      cur_position[1] <- as.character(as.integer(cur_position[1]) + 1)
      path_matrix[cur_position[1], cur_position[2]] <- "o"
    } else if (cell_below_l == ".") {
      # Move the sand down by updating the current position
      path_matrix[cur_position[1], cur_position[2]] <- "."
      cur_position[1] <- as.character(as.integer(cur_position[1]) + 1)
      cur_position[2] <- as.character(as.integer(cur_position[2]) - 1)
      path_matrix[cur_position[1], cur_position[2]] <- "o"
    } else if (cell_below_r == ".") {
      # Move the sand down by updating the current position
      path_matrix[cur_position[1], cur_position[2]] <- "."
      cur_position[1] <- as.character(as.integer(cur_position[1]) + 1)
      cur_position[2] <- as.character(as.integer(cur_position[2]) + 1)
      path_matrix[cur_position[1], cur_position[2]] <- "o"
    } else {
      # Sand cannot fall further, so back to source
      cur_position <- c("0", "500")
      # Visualize the updated path matrix
      imgs <- visualize_path_matrix(path_matrix, imgs)
    }
  }

  print(imgs)
  return(image_animate(imgs, fps = 20, optimize = TRUE))
})
