# You can find a list of CRAN mirrors at
# https://cran.r-project.org/mirrors.html
options(repos = "https://cloud.r-project.org/")
# Install the image magick package if not already installed
if (!require(magick)) install.packages("magick")

library(magick)

read_file_lines <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(paste("File", file_path, "does not exist."))
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

filename_generator <- function(ext) {
  # Global counter variable
  counter <- 1
  # Function to generate a new filename with counter
  return(\() {
    filename <- paste0("image_", sprintf("%03d", counter), ".", ext)
    counter <<- counter + 1
    return(filename)
  })
}

generate_filename <- filename_generator("png")

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

generate_gif_filename <- filename_generator("gif")
prepare_and_clean <- function(fn, cave_matrix, no_gif) {
  if (no_gif) {
    fn(cave_matrix, no_gif)
  } else {
    build_path <- "build"
    # Create the build folder if it doesn't exist
    if (!file.exists(build_path)) {
      dir.create(build_path)
    }
    # Move into the build folder
    setwd(build_path)

    # execute main part
    animation <- fn(cave_matrix, no_gif)

    # Move into the parent folder
    setwd("..")
    image_write(animation, generate_gif_filename())
    unlink(build_path, recursive = TRUE)
  }
}

simulate_sand <- function(cave_matrix, current, sand_units, fn) {
  # Check the cell below the current position
  if (cave_matrix[current$row + 1, current$col] == ".") {
    # Move the sand down by updating the current position
    cave_matrix[current$row, current$col] <- "."
    current$row <- current$row + 1
    cave_matrix[current$row, current$col] <- "o"
  } else if (cave_matrix[current$row + 1, current$col - 1] == ".") {
    cave_matrix[current$row, current$col] <- "."
    current$row <- current$row + 1
    current$col <- current$col - 1
    cave_matrix[current$row, current$col] <- "o"
  } else if (cave_matrix[current$row + 1, current$col + 1] == ".") {
    cave_matrix[current$row, current$col] <- "."
    current$row <- current$row + 1
    current$col <- current$col + 1
    cave_matrix[current$row, current$col] <- "o"
  } else {
    # Sand cannot fall further, so back to source
    current <- source(cave_matrix)
    cave_matrix[current$row, current$col] <- "+"
    sand_units <- sand_units + 1
    fn(cave_matrix)
  }

  return(list(
    cave_matrix = cave_matrix,
    current = current,
    sand_units = sand_units
  ))
}

is_end_part2 <- function(cave_matrix, current) {
  source_ <- source(cave_matrix)
  if (current$row == source_$row && current$col == source_$col) {
    if (cave_matrix[current$row + 1, current$col] == "o" &&
        cave_matrix[current$row + 1, current$col - 1] == "o" &&
        cave_matrix[current$row + 1, current$col + 1] == "o"
        ) {
      return(TRUE)
    }
  }
  return(FALSE)
}

part1 <- function(cave_matrix, no_gif) {
  sand_units <- 0
  imgs <- c()
  if (!no_gif) {
    imgs <- visualize_cave(cave_matrix, imgs)
  }

  current <- source(cave_matrix)
  while (TRUE) {
    if (current$row == nrow(cave_matrix) ||
      current$col == ncol(cave_matrix) ||
      current$col == 1) {
      break # Exit the loop when sand reaches the bottom
    }

    res <- simulate_sand(
      cave_matrix, current, sand_units,
      \(cave_matrix) {
        if (!no_gif) {
          # Visualize the updated cave matrix
          imgs <<- visualize_cave(cave_matrix, imgs)
        }
      }
    )
    cave_matrix <- res$cave_matrix
    current <- res$current
    sand_units <- res$sand_units
  }

  cat(paste("Part 1:", sand_units, "\n"))
  if (!no_gif) {
    return(image_animate(imgs, fps = 25, optimize = TRUE))
  }
}

source <- function(cave_matrix) {
  list(
    row = which(rownames(cave_matrix) == "0"),
    col = which(colnames(cave_matrix) == "500")
  )
}

part2 <- function(cave_matrix, no_gif) {
  sand_units <- 0
  imgs <- c()
  if (!no_gif) {
    imgs <- visualize_cave(cave_matrix, imgs)
  }

  last_row_name <- as.integer(rownames(cave_matrix)[nrow(cave_matrix)])
  cave_matrix <- rbind(
    cave_matrix,
    matrix(".", nrow = 1, ncol = ncol(cave_matrix))
  )
  rownames(cave_matrix)[nrow(cave_matrix)] <- last_row_name + 1
  cave_matrix <- rbind(
    cave_matrix,
    matrix("#", nrow = 1, ncol = ncol(cave_matrix))
  )
  rownames(cave_matrix)[nrow(cave_matrix)] <- last_row_name + 2

  current <- source(cave_matrix)
  while (TRUE) {
    if (current$col == 2) {
      cave_matrix <- cbind(rep(".", times = nrow(cave_matrix)), cave_matrix)
      cave_matrix[nrow(cave_matrix), 1] <- "#"
      colnames(cave_matrix)[1] <-
        as.integer(colnames(cave_matrix)[2]) - 1
      current$col <- current$col + 1
    }

    if (current$col == ncol(cave_matrix) - 1) {
      cave_matrix <- cbind(cave_matrix, rep(".", times = nrow(cave_matrix)))
      cave_matrix[nrow(cave_matrix), ncol(cave_matrix)] <- "#"
      colnames(cave_matrix)[ncol(cave_matrix)] <-
        as.integer(colnames(cave_matrix)[ncol(cave_matrix) - 1]) + 1
    }

    if (is_end_part2(cave_matrix, current)) {
      break
    }

    res <- simulate_sand(
      cave_matrix, current, sand_units,
      \(cave_matrix) {
        if (!no_gif) {
          # Visualize the updated cave matrix
          imgs <<- visualize_cave(cave_matrix, imgs)
        }
      }
    )
    cave_matrix <- res$cave_matrix
    current <- res$current
    sand_units <- res$sand_units
  }

  cat(paste("Part 2:", sand_units + 1, "\n"))
  if (!no_gif) {
    return(image_animate(imgs, fps = 25, optimize = TRUE))
  }
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("Usage: Rscript main.R <input-path> --no-gif")
}
file_path <- args[1]
no_gif <- "--no-gif" %in% args
lines <- read_file_lines(file_path)
slices <- parse_slices(lines)
cave_matrix <- create_cave_matrix(slices)
cave_matrix["0", "500"] <- "+"

prepare_and_clean(part1, cave_matrix, no_gif)
prepare_and_clean(part2, cave_matrix, no_gif)
