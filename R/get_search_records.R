#' Get User Records
#'
#' This function sends a request to a specified URL, appending necessary details in the URL path,
#' and fetches user records based on the count parameter. If count is more than 100, additional requests
#' are sent out in batches of 100 until the count is fulfilled.
#'
#' @param request The request URL.
#' @param key The API key.
#' @param repo The repository.
#' @param collection The collection.
#' @param count The number of records to fetch. If `count` is greater than 100, the function will send multiple requests in batches of 100.
#' @param throttle A vector specifying the rate limit as `c(requests, seconds)`. If the number of requests is divisible by `requests`, the function will pause for `seconds` seconds. Defaults to `c(3000, 300)`.
#'
#' @return A list of responses. Each response is a list representing the JSON response from each request.
#'
#' @export
#' @importFrom httr2 request req_headers req_url_path_append req_perform resp_body_json
#' @importFrom glue glue
#' @importFrom purrr map
get_search_records <- function(request, key, search, count, throttle = c(3000, 300)) {
  # Calculate the limit for the first request
  limit <- min(count, 100)

  # Get initial response
  response <- httr2::request(request) %>%
    httr2::req_headers("Authorization" = paste("Bearer", key)) %>%
    httr2::req_url_path_append(glue::glue("?q={search}&limit={limit}")) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  if (count <= 100) {
    return(response)
  } else {
    # Calculate how many more requests are needed
    iter <- (count - limit) / 100
    iter <- seq_len(iter)

    # Set initial cursor value
    cursor <- response$cursor

    # Initialize a list to store the responses
    # Initialize a list to store the responses
    req <- list()

    # Define the function you want to use safely
    safe_request <- purrr::safely(function(i, cursor) {
      # If the current iteration number is divisible by the throttle value, pause for the defined throttle seconds
      if (i %% throttle[[1]] == 0) {
        Sys.sleep(throttle[[2]])
      }

      # Send a request and get a response
      req_response <- httr2::request(request) %>%
        httr2::req_headers("Authorization" = paste("Bearer", key)) %>%
        httr2::req_url_path_append(
          glue::glue(
            "?q={search}&limit=100&cursor={cursor}"
          )
        ) %>%
        httr2::req_perform() %>%
        httr2::resp_body_json()

      # Return the response
      req_response
    })

    # Start a for loop
    for (i in seq_along(iter)) {
      # Make the request safely
      result <- safe_request(i, cursor)

      # If there was an error, handle it
      if (!is.null(result$error)) {
        print(result$error)
        # Optionally, break the loop or skip to the next iteration
        # break or next
      } else {
        # Update the cursor for the next iteration
        cursor <- result$result$cursor
        # Store the response in the list
        req[[i]] <- result$result
      }
    }

    # Combine all responses
    all_responses <- c(list(response), req)
    return(all_responses)
  }
}

flatten_search <- function(x) {
  x %>% purrr::map(tibble::as_tibble) %>%
    dplyr::bind_rows() %>%
    dplyr::select(posts) %>%
    tidyr::unnest_wider(posts)
}
