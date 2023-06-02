# You can find a list of CRAN mirrors at
# https://cran.r-project.org/mirrors.html
options(repos = "https://cloud.r-project.org/")
# Install the image magick package if not already installed
if (!require(magick)) install.packages("magick")

library(magick)

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

parse_slices <- function(lines) {
  slices <- list()
  for (line in lines) {
    # Split the line into individual points
    points <- strsplit(line, "->")[[1]]
    # Create an empty matrix to store the slice coordinates
    slice <-
      matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c("x", "y")))

    for (point in points) {
      # Split each point into x and y coordinates
      xy <- strsplit(point, ",")[[1]]
      x <- xy[1]
      y <- xy[2]
      # Add the coordinates to the slice matrix
      slice <- rbind(slice, c(as.integer(x), as.integer(y)))
    }
    # Add the slice matrix to the list of slices
    slices <- append(slices, list(slice))
  }
  return(slices)
}

create_cave_matrix <- function(slices) {
  max_y <- max(sapply(slices, function(slice) max(slice[, "y"])))
  max_x <- max(sapply(slices, function(slice) max(slice[, "x"])))
  min_x <- min(sapply(slices, function(slice) min(slice[, "x"])))
  x_size <- max_x - min_x

  # Create an empty matrix with the determined dimensions
  cave_matrix <- matrix(".", nrow = max_y + 1, ncol = x_size + 1)
  col_names <- min_x:max_x
  row_names <- 0:max_y
  dimnames(cave_matrix) <- list(row_names, col_names)

  # Populate the matrix with lines from slices
  for (slice in slices) {
    prev_x <- slice[1, "x"]
    prev_y <- slice[1, "y"]
    for (i in seq_len(nrow(slice))) {
      x <- slice[i, "x"]
      y <- slice[i, "y"]
      if (x == prev_x) {
        # Vertical line
        y_seq <- as.character(min(y, prev_y):max(y, prev_y))
        cave_matrix[y_seq, as.character(x)] <- "#"
      } else if (y == prev_y) {
        # Horizontal line
        x_seq <- as.character(min(x, prev_x):max(x, prev_x))
        cave_matrix[as.character(y), x_seq] <- "#"
      }
      prev_x <- x
      prev_y <- y
    }
  }

  return(cave_matrix)
}

filename_generator <- function() {
  # Global counter variable
  counter <- 1
  # Function to generate a new filename with counter
  return(function() {
    filename <- paste0("image_", sprintf("%03d", counter), ".png")
    counter <<- counter + 1
    return(filename)
  })
}

generate_filename <- filename_generator()

visualize_cave <- function(cave_matrix, imgs) {
  nrow <- nrow(cave_matrix)
  ncol <- ncol(cave_matrix)
  filename <- generate_filename()
  # Save the plot as a PNG image
  png(filename)
  # Set up the plot
  plot(1, 1,
    type = "n",
    xlim = c(1, ncol), ylim = c(1, nrow),
    xlab = "x", ylab = "y", axes = FALSE
  )
  # Set the x-axis labels to column names
  axis(1, at = 1:ncol, labels = colnames(cave_matrix))
  # Set the y-axis labels to row names
  axis(2, at = 1:nrow, labels = rev(rownames(cave_matrix)))

  # Add the lines to the plot
  for (i in 1:nrow) {
    for (j in 1:ncol) {
      switch(cave_matrix[i, j],
        "#" = {
          if (i < nrow && cave_matrix[i + 1, j] == "#") {
            # Vertical line
            lines(c(j, j), c(nrow - i + 1, nrow - i), col = "black")
          }
          # Check if the cell to the right is also a line to avoid duplicate
          if (j < ncol && cave_matrix[i, j + 1] == "#") {
            # Horizontal line
            lines(c(j, j + 1), c(nrow - i + 1, nrow - i + 1), col = "black")
          }
        },
        "+" = {
          # Draw a source
          points(j, nrow - i + 1, col = "red", pch = "+", cex = 1)
        },
        "o" = {
          # Draw a sand
          points(j, nrow - i + 1, col = "orange", pch = 24, cex = 0.8)
        }
      )
    }
  }
  dev.off()
  image <- image_read(filename)
  return(append(imgs, image))
}

main <- function(fn) {
  args <- commandArgs(trailingOnly = TRUE)
  lines <- read_file_lines(args)
  slices <- parse_slices(lines)
  cave_matrix <- create_cave_matrix(slices)
  cave_matrix["0", "500"] <- "+"

  build_path <- "build"
  # Create the build folder if it doesn't exist
  if (!file.exists(build_path)) {
    dir.create(build_path)
  }
  # Move into the build folder
  setwd(build_path)

  # execute main part
  animation <- fn(cave_matrix)

  # Move into the parent folder
  setwd("..")
  image_write(animation, "result.gif")
  unlink(build_path, recursive = TRUE)
}

part1 <- function(cave_matrix) {
  imgs <- c()
  imgs <- visualize_cave(cave_matrix, imgs)
  # Get the names of the first and last column
  first_col <- as.integer(colnames(cave_matrix)[1])
  last_col <- as.integer(colnames(cave_matrix)[ncol(cave_matrix)])
  last_row <- as.integer(rownames(cave_matrix)[nrow(cave_matrix)])

  cur_position <- c("0", "500")
  while (TRUE) {
    if (cur_position[1] == last_row ||
      cur_position[2] == first_col ||
      cur_position[2] == last_col) {
      break # Exit the loop when sand reaches the bottom
    }

    # Check the cell below the current position
    cell_below <- cave_matrix[as.character(as.integer(cur_position[1]) + 1), cur_position[2]]
    cell_below_l <- cave_matrix[as.character(as.integer(cur_position[1]) + 1), as.character(as.integer(cur_position[2]) - 1)]
    cell_below_r <- cave_matrix[as.character(as.integer(cur_position[1]) + 1), as.character(as.integer(cur_position[2]) + 1)]
    if (cell_below == ".") {
      # Move the sand down by updating the current position
      cave_matrix[cur_position[1], cur_position[2]] <- "."
      cur_position[1] <- as.character(as.integer(cur_position[1]) + 1)
      cave_matrix[cur_position[1], cur_position[2]] <- "o"
    } else if (cell_below_l == ".") {
      # Move the sand down by updating the current position
      cave_matrix[cur_position[1], cur_position[2]] <- "."
      cur_position[1] <- as.character(as.integer(cur_position[1]) + 1)
      cur_position[2] <- as.character(as.integer(cur_position[2]) - 1)
      cave_matrix[cur_position[1], cur_position[2]] <- "o"
    } else if (cell_below_r == ".") {
      # Move the sand down by updating the current position
      cave_matrix[cur_position[1], cur_position[2]] <- "."
      cur_position[1] <- as.character(as.integer(cur_position[1]) + 1)
      cur_position[2] <- as.character(as.integer(cur_position[2]) + 1)
      cave_matrix[cur_position[1], cur_position[2]] <- "o"
    } else {
      # Sand cannot fall further, so back to source
      cur_position <- c("0", "500")
      cave_matrix[cur_position[1], cur_position[2]] <- "+"
      # Visualize the updated path matrix
      imgs <- visualize_cave(cave_matrix, imgs)
    }
  }
  return(image_animate(imgs, fps = 20, optimize = TRUE))
}

main(part1)
