---
type: scenario
name: vector-extension-basic
feature: vector-extension
covers:
  - core
---

# vector-extension Basic Scenario

## Given
- A database with extension loading enabled
- The sqlite-vec extension (vec0.so) available on the filesystem
- A need to store and search high-dimensional vector embeddings

## When
- Loading the vec0 extension using load-extension
- Creating virtual tables for vector storage using create-vector-table
- Defining vector columns with specific dimensions (e.g., embedding with 4 or 128 dimensions)
- Inserting vector data as JSON strings ('[0.1, 0.2, 0.3, 0.4]')
- Performing k-nearest-neighbor searches using vector-search
- Passing query vectors as JSON strings or Lisp float arrays
- Using vector scalar functions (vec-add, vec-distance-L2, vec-normalize)

## Then
- The extension loads successfully without errors
- Virtual tables are created with the correct vector column dimensions
- Vectors are stored and retrieved correctly
- K-NN searches return the closest vectors ranked by distance
- Distance calculations are accurate (e.g., identical vectors have 0.0 distance)
- Vector arithmetic functions (add, normalize) produce correct results
- Both string and array formats for vectors are supported
- Search results include rowid and distance, sorted by ascending distance
