tag <- c("C99", "A100", "A20", "5")
tag.order <-  c(4, 3, 2, 1)
tag.sort <- c("C000000099", "A000000100", "A000000020", "0000000005")


tag.primary <- c("10", "A100", "A20", "5")
tag.primary.order <- c(4, 1, 3, 2)

df <- data.frame(
  tag_primary = tag.primary,
  tag = tag
)

reset_rownames <- function(x) {
  row.names(x) <- NULL
  x
}

test_that("tag_sort can sort and arrange by different columns as expected", {
  expect_equal(
    tag_sort(df, tag),
    reset_rownames(df[tag.order, ])
  )

  expect_equal(
    tag_sort(df, tag_primary),
    reset_rownames(df[tag.primary.order, ])
  )
})

test_that("vec_tag_sort generates the proper tag sort vector", {
  expect_equal(vec_tag_sort(tag), tag.sort)
})


test_that("tag_sort drop and arrange args function as expected", {
  df.with.sort <- dplyr::mutate(df, tag_sort = tag.sort)

  expect_equal(
    tag_sort(df, tag, .arrange = FALSE),
    df
  )
  expect_equal(
    tag_sort(df, tag, .arrange = FALSE, .drop = FALSE),
    df.with.sort
  )

  expect_equal(
    tag_sort(df, tag, .arrange = TRUE, .drop = FALSE),
    reset_rownames(df.with.sort[tag.order, ])
  )
})
