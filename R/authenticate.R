#' Get the DID (Decentralized Identifier) for a handle
#'
#' This function sends a request to the specified URL, appends the provided handle to the URL path,
#' performs the request, and returns the DID found in the response body.
#'
#' @param fed The [ATProto](https://atproto.com) instance to which the handle belongs.
#' @param handle The handle for which to get the DID.
#' @return The DID corresponding to the provided handle.
#' @export
get_did <- function(fed="bsky.social", handle) {
  response <-
    httr2::request(glue::glue("https://{fed}/xrpc/com.atproto.identity.resolveHandle")) %>%
    httr2::req_url_path_append(glue::glue("?handle={handle}")) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  return(response[['did']])
}

#' Get the API key for a DID and app password
#'
#' This function sends a request to the specified URL, includes the provided DID and password in the
#' request body, performs the request, and returns the API key found in the response body.

#' @param fed The [ATProto](https://atproto.com) instance to which the handle belongs.
#' @param did The DID for which to get the API key.
#' @param app_password The password to include in the request.
#' @return The API key corresponding to the provided DID and password.
#' @export
get_api <- function(fed, did, app_password) {
  api_key_url <-
    httr2::request(glue::glue("https://{fed}/xrpc/com.atproto.server.createSession")) %>%
    httr2::req_body_json(list(identifier = did, password = app_password)) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  return(api_key_url[['accessJwt']])
}
