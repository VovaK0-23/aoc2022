#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_STACKS 10
#define MAX_CRATES 500

// Structure to represent a stack of crates
typedef struct {
  char crates[MAX_CRATES];  // Array to store the crates in the stack
  int top;                  // Index of the topmost crate in the stack
} Stack;

typedef struct {
  char** lines;
  int line_count;
} FileContents;

void push(Stack* s, char* crates, int num_crates) {
  if ((s->top + num_crates) > MAX_CRATES) {
    printf("Stack overflow");
    exit(1);
  }
  for (int i = 0; i < num_crates; i++) {
    s->crates[s->top] = crates[i];
    s->top += 1;
  }
}

void pop(Stack* s, char* crates, int num_crates) {
  if (s->top - num_crates < 0) {
    printf("Stack underflow");
    exit(1);
  }
  for (int i = num_crates - 1; i >= 0; i--) {
    s->top -= 1;
    crates[i] = s->crates[s->top];
  }
}

FileContents read_lines(char* filename) {
  FILE* fp = fopen(filename, "r");
  FileContents results;

  size_t len;
  ssize_t read;
  char* line = NULL;
  int array_size = 5000;
  char** lines = malloc(array_size * sizeof(char*));
  int count = 0;

  if (fp == NULL) {
    printf("Error: Could not open file %s\n", filename);
    exit(1);
  }

  while ((read = getline(&line, &len, fp)) != -1) {
    char* p;
    // strip newline - find it in string and replace it with null terminator
    if ((p = strchr(line, '\n')) != NULL) {
      *p = '\0';
      read = read - 1;
    }
    if (count >= array_size) {
      array_size = array_size * 2;
      lines = realloc(lines, array_size * sizeof(char*));
    }
    lines[count] = strdup(line);
    count = count + 1;
  }

  fclose(fp);
  results.lines = lines;
  results.line_count = count;
  return results;
}

int count_rows(FileContents fc) {
  int row_count = 0;
  for (int i = 0; i < fc.line_count; i++) {
    if (row_count == 0 && strlen(fc.lines[i]) == 0) {
      row_count = i - 1;
      break;
    }
  }
  return row_count;
}

void init_stack(FileContents fc, Stack* s, int row_count, int col) {
  s->top = 0;
  for (int row = row_count - 1; row >= 0; row--) {
    int col_pos = 1 + (col * 4);
    char c = fc.lines[row][col_pos];
    char chars[1] = {c};
    if (c != ' ') {
      push(s, chars, 1);
    }
  }
}

void part1(char* filename) {
  FileContents fc = read_lines(filename);
  int row_count = count_rows(fc);

  // Initialize stacks from input
  Stack stacks[MAX_STACKS];  // Array to store all stacks
  int num_stacks = (strlen(fc.lines[0]) + 1) / 4;
  for (int col = 0; col < num_stacks; col++) {
    init_stack(fc, &stacks[col], row_count, col);
  }

  int offset = row_count + 2;
  int move_count = fc.line_count - offset;
  for (int i = 0; i < move_count; i++) {
    int stack_from, stack_to;  // Stacks involved in the action
    int num_crates;            // Number of crates to move
    char* move_line = strdup(fc.lines[offset + i]);
    sscanf(move_line, "move %d from %d to %d", &num_crates, &stack_from, &stack_to);
    stack_from--;  // Adjust for 0-indexing
    stack_to--;

    for (int j = 0; j < num_crates; j++) {
      char crates[num_crates];
      pop(&stacks[stack_from], crates, 1);
      push(&stacks[stack_to], crates, 1);
    }
  }

  // Print the top crate of each stack
  printf("Part 1: ");
  for (int col = 0; col <= num_stacks; col++) {
    printf("%c", stacks[col].crates[stacks[col].top - 1]);
  }
  printf("\n");
}

void part2(char* filename) {
  FileContents fc = read_lines(filename);
  int row_count = count_rows(fc);

  // Initialize stacks from input
  Stack stacks[MAX_STACKS];  // Array to store all stacks
  int num_stacks = (strlen(fc.lines[0]) + 1) / 4;
  for (int col = 0; col < num_stacks; col++) {
    init_stack(fc, &stacks[col], row_count, col);
  }

  int offset = row_count + 2;
  int move_count = fc.line_count - offset;
  for (int i = 0; i < move_count; i++) {
    int stack_from, stack_to;  // Stacks involved in the action
    int num_crates;            // Number of crates to move
    char* move_line = strdup(fc.lines[offset + i]);
    sscanf(move_line, "move %d from %d to %d", &num_crates, &stack_from, &stack_to);
    stack_from--;  // Adjust for 0-indexing
    stack_to--;

    char crates[num_crates];
    pop(&stacks[stack_from], crates, num_crates);
    push(&stacks[stack_to], crates, num_crates);
  }

  // Print the top crate of each stack
  printf("Part 2: ");
  for (int col = 0; col <= num_stacks; col++) {
    printf("%c", stacks[col].crates[stacks[col].top - 1]);
  }
  printf("\n");
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    printf("Usage: %s <input_file>\n", argv[0]);
    return 1;
  }
  part1(argv[1]);
  part2(argv[1]);

  return 0;
}
