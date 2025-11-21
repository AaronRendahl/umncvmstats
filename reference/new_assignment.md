# Create a blank new assignment

Create a blank new assignment from the included quarto template.

## Usage

``` r
new_assignment(
  filename,
  title,
  author,
  open = is_interactive() & isAvailable()
)
```

## Arguments

- filename:

  desired filename of new file.

- title:

  desired title, to be inserted into new file.

- author:

  desired author, to be inserted into new file.

- open:

  a logical specifying whether or not to open the new file for editing.
