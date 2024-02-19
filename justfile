# Default task
default: build

# Build the project
build:
    stack build

# Build the project 
build-watch:
    stack build --file-watch --pedantic
# Run the project
run:
    stack run

# Test the project
test:
    stack test

# Clean the project
clean:
    stack clean --full

# Generate documentation
docs:
    stack haddock