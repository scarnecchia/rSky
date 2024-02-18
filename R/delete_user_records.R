delete_user_records <-
  function(key, repo, collection, rkey) {
    # Get initial response
    httr2::request("https://bsky.social/xrpc/com.atproto.repo.deleteRecord") %>%
      httr2::req_headers("Authorization" = paste("Bearer", key)) %>%
      httr2::req_body_json(list(
        repo = repo,
        collection = collection,
        rkey = rkey
      )) %>%
      httr2::req_perform()
  }

get_rkey <- function(uri) {
    stringr::str_extract(uri, "(?<=/)[^/]+$")
}
