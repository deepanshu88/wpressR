#' Make API Requests to WordPress REST API
#'
#' This function performs HTTP requests to the WordPress REST API, supporting various HTTP methods.
#'
#' @param method Character. The HTTP method to be used for the request (e.g., "GET", "POST", "PUT", "DELETE").
#' @param wp_site Character. The base URL of the WordPress site (e.g., "https://example.com").
#' @param endpoint Character. The specific endpoint to which the request will be made (e.g., "wp/v2/posts").
#' @param wp_header List. Authentication headers created using `add_headers(Authorization = paste("Basic", wp_token))`.
#' @param data Optional. A list containing data to be sent with the request (for "POST" or "PUT" requests). Default is NULL.
#'
#' @return A list containing the response from the API request or an error message if the request fails.
#'
#' @details This function is a general-purpose utility for making API requests to the WordPress REST API. It abstracts the underlying `httr` functions, allowing users to easily interact with various endpoints.
#'
#' @examples
#' \dontrun{
#' wp_header <- httr::add_headers(Authorization = paste("Basic", wp_token))
#'
#' # Make a GET request to fetch posts
#' response <- wp_api_request("GET", wp_site = "https://example.com", endpoint = "wp/v2/posts", wp_header = wp_header)
#'
#' # Make a POST request to create a new post
#' new_post_data <- list(title = "New Post", content = "This is the content of the new post.")
#' response <- wp_api_request("POST", wp_site = "https://example.com", endpoint = "wp/v2/posts", wp_header = wp_header, data = new_post_data)
#' }
#'
#' @export
wp_api_request <- function(method, wp_site, endpoint, wp_header, data = NULL) {
  api_url <- paste0(wp_site, endpoint)

  response <- switch(method,
                     GET = GET(api_url, wp_header, content_type_json()),
                     POST = POST(api_url, wp_header, body = toJSON(data, auto_unbox = TRUE), encode = "json", content_type_json()),
                     PUT = PUT(api_url, wp_header, body = toJSON(data, auto_unbox = TRUE), encode = "json", content_type_json()),
                     DELETE = DELETE(api_url, wp_header, content_type_json())
  )

  return(response)
}

#' Get WordPress Page or Post Content
#'
#' Retrieves a page or post from a WordPress site using the REST API.
#'
#' @param id Numeric or `NULL`. The ID of the specific page or post to retrieve. If `NULL`, retrieves all content of the specified `content_type`.
#' @param wp_site Character. The base URL of the WordPress site (e.g., "https://example.com").
#' @param wp_header List. A named list containing the authorization headers required for API requests.
#' @param content_type Character. The type of content to retrieve, either "posts" (default) or "pages".
#'
#' @return A list containing the details of the retrieved content or an error message if the request fails.
#'
#' @details This function interacts with the WordPress REST API to retrieve content. By default, it fetches "posts", but setting `content_type = "pages"` will fetch pages instead. If `id` is provided, it retrieves a specific page or post by its ID. Otherwise, it retrieves a list of all posts or pages.
#'
#' @examples
#' \dontrun{
#' # Fetch all posts
#' get_content(wp_site = "https://example.com", wp_header = add_headers(Authorization = paste("Basic", wp_token)))
#'
#' # Fetch a specific page by ID
#' get_content(id = 5, wp_site = "https://example.com", wp_header = add_headers(Authorization = paste("Basic", wp_token)), content_type = "pages")
#' }
#'
#' @export
get_content <- function(id = NULL, wp_site, wp_header, content_type = "posts") {

  if (is.null(id)) {
    # Fetch all posts or pages if ID is NULL
    endpoint <- paste0("/wp-json/wp/v2/", content_type)
  } else {
    # Fetch specific post or page if ID is provided
    endpoint <- paste0("/wp-json/wp/v2/", content_type, "/", id)
  }

  response <- wp_api_request("GET", wp_site, endpoint, wp_header)

  if (status_code(response) == 200) {
    return(content(response, "parsed", simplifyVector = TRUE))
  } else {
    stop(paste("Error fetching content:", status_code(response), content(response, "text")))
  }
}

#' Post Content to WordPress
#'
#' Creates a new post or page on a WordPress site using the REST API.
#'
#' @param wp_site Character. The base URL of the WordPress site (e.g., "https://example.com").
#' @param wp_header List. A named list created using `add_headers(Authorization = paste("Basic", wp_token))` for authentication, where `wp_token` is the base64-encoded username and password for the WordPress site.
#' @param data List. A list containing the content data to be posted. The list must include required fields such as `title` and `content`.
#' @param content_type Character. The type of content to create, either "posts" (default) or "pages".
#'
#' @return A list containing the details of the newly created content, or an error message if the request fails.
#'
#' @details This function allows the creation of new posts or pages on a WordPress site via the REST API. The `wp_header` parameter must be passed using `add_headers(Authorization = paste("Basic", wp_token))`, where `wp_token` is the base64-encoded string of your username and password. The `data` list should contain all necessary fields like `title`, `content`, and any other post attributes.
#'
#' @examples
#' \dontrun{
#' # Post a new blog post
#' wp_header <- httr::add_headers(Authorization = paste("Basic", wp_token))
#' data <- list(title = "New Blog Post", content = "This is the content of the post.")
#' post_content(wp_site = "https://example.com", wp_header = wp_header, data = data)
#'
#' # Post a new page
#' post_content(wp_site = "https://example.com", wp_header = wp_header, data = data, content_type = "pages")
#' }
#'
#' @export
post_content <- function(wp_site, wp_header, data, content_type = "posts") {
  endpoint <- paste0("/wp-json/wp/v2/", content_type)
  response <- wp_api_request("POST", wp_site, endpoint, wp_header, data)

  if (status_code(response) == 201) {
    return(content(response, "parsed", simplifyVector = TRUE))
  } else {
    stop(paste("Error creating content:", status_code(response), content(response, "text")))
  }
}

#' Update WordPress Page or Post Content
#'
#' Updates an existing page or post on a WordPress site using the REST API.
#'
#' @param id Numeric. The ID of the page or post to be updated.
#' @param wp_site Character. The base URL of the WordPress site (e.g., "https://example.com").
#' @param wp_header List. A named list created using `add_headers(Authorization = paste("Basic", wp_token))` for authentication, where `wp_token` is the base64-encoded username and password for the WordPress site.
#' @param data List. A list containing the updated content data. The list should include fields that need to be updated, such as `title` or `content`.
#' @param content_type Character. The type of content to update, either "posts" (default) or "pages".
#'
#' @return A list containing the details of the updated content, or an error message if the request fails.
#'
#' @details This function allows users to update existing posts or pages on a WordPress site via the REST API. The `wp_header` parameter must be passed using `add_headers(Authorization = paste("Basic", wp_token))`, where `wp_token` is the base64-encoded string of your username and password. The `data` list should contain the fields that need to be updated for the specified post or page.
#'
#' @examples
#' \dontrun{
#' # Update an existing blog post
#' wp_header <- httr::add_headers(Authorization = paste("Basic", wp_token))
#' data <- list(title = "Updated Blog Post Title", content = "This is the updated content of the post.")
#' modify_content(id = 5, wp_site = "https://example.com", wp_header = wp_header, data = data)
#'
#' # Update an existing page
#' modify_content(id = 3, wp_site = "https://example.com", wp_header = wp_header, data = data, content_type = "pages")
#' }
#'
#' @export
modify_content <- function(id, wp_site, wp_header, data, content_type = "posts") {
  endpoint <- paste0("/wp-json/wp/v2/", content_type, "/", id)
  response <- wp_api_request("PUT", wp_site, endpoint, wp_header, data)

  if (status_code(response) == 200) {
    return(content(response, "parsed", simplifyVector = TRUE))
  } else {
    stop(paste("Error updating content:", status_code(response), content(response, "text")))
  }
}

#' Delete WordPress Page or Post Content
#'
#' Deletes a specific page or post on a WordPress site using the REST API.
#'
#' @param id Numeric. The ID of the page or post to be deleted.
#' @param wp_site Character. The base URL of the WordPress site (e.g., "https://example.com").
#' @param wp_header List. A named list created using `add_headers(Authorization = paste("Basic", wp_token))` for authentication, where `wp_token` is the base64-encoded username and password for the WordPress site.
#' @param content_type Character. The type of content to delete, either "posts" (default) or "pages".
#'
#' @return A list containing a confirmation message or an error message if the request fails.
#'
#' @details This function allows users to delete existing posts or pages on a WordPress site via the REST API. The `wp_header` parameter must be passed using `add_headers(Authorization = paste("Basic", wp_token))`, where `wp_token` is the base64-encoded string of your username and password.
#'
#' @examples
#' \dontrun{
#' # Delete a blog post
#' wp_header <- httr::add_headers(Authorization = paste("Basic", wp_token))
#' delete_content(id = 5, wp_site = "https://example.com", wp_header = wp_header)
#'
#' # Delete a page
#' delete_content(id = 3, wp_site = "https://example.com", wp_header = wp_header, content_type = "pages")
#' }
#'
#' @export
delete_content <- function(id, wp_site, wp_header, content_type = "posts") {
  endpoint <- paste0("/wp-json/wp/v2/", content_type, "/", id)
  response <- wp_api_request("DELETE", wp_site, endpoint, wp_header)

  if (status_code(response) == 200) {
    return(content(response, "parsed", simplifyVector = TRUE))
  } else {
    stop(paste("Error deleting content:", status_code(response), content(response, "text")))
  }
}

#' Fetch WordPress Posts with Pagination
#'
#' Retrieves a list of posts from a WordPress site, with support for pagination.
#'
#' @param wp_site Character. The base URL of the WordPress site (e.g., "https://example.com").
#' @param wp_header List. A named list created using `add_headers(Authorization = paste("Basic", wp_token))` for authentication, where `wp_token` is the base64-encoded username and password for the WordPress site.
#' @param content_type Character. The type of content to delete, either "posts" (default) or "pages".
#'
#' @return A list containing the posts retrieved for the specified page, or an error message if the request fails.
#'
#' @details This function allows users to fetch posts from a WordPress site with pagination. By specifying the `per_page` and `page` parameters, users can control how many posts to retrieve and from which page. If the `wp_header` is provided, the function will include authentication in the API request.
#'
#' @examples
#' \dontrun{
#' # Fetch the first page of posts with the default number per page
#' get_content_pagination(wp_site = "https://example.com", wp_header = wp_header, content_type = "pages")
#' }
#'
#' @export
get_content_pagination <- function(wp_site, wp_header, content_type = "posts") {
  # Construct the API URL based on content_type
  api_url <- paste0(wp_site, "/wp-json/wp/v2/", content_type, "?page=1&per_page=100")
  response <- GET(api_url, wp_header)

  # Get total pages from response headers
  pages_count <- headers(response)[['X-WP-TotalPages']]
  total_pages <- as.numeric(pages_count)

  current_page <- 1
  all_page_items_json <- list()  # Initialize an empty list to store all items

  while (current_page <= total_pages) {
    api_url <- paste0(wp_site, "/wp-json/wp/v2/", content_type, "?page=", current_page, "&per_page=100")
    page_items <- GET(api_url, wp_header)
    page_items_json <- content(page_items, "parsed", simplifyVector = TRUE)

    # Append the current page's items to the list
    all_page_items_json <- c(all_page_items_json, page_items_json)
    current_page <- current_page + 1
  }

  return(all_page_items_json)
}

#' Upload Image to WordPress Media Library
#'
#' Uploads an image from the local drive to the WordPress media library using the REST API.
#'
#' @param image_path Character. The file path of the image to be uploaded (e.g., "path/to/image.jpg").
#' @param wp_site Character. The base URL of the WordPress site (e.g., "https://example.com").
#' @param wp_header List. A named list created using `add_headers(Authorization = paste("Basic", wp_token))` for authentication, where `wp_token` is the base64-encoded username and password for the WordPress site.
#'
#' @return A list containing the details of the uploaded media, or an error message if the upload fails.
#'
#' @details This function allows users to upload images directly to the WordPress media library via the REST API. The `wp_header` parameter must be passed using `add_headers(Authorization = paste("Basic", wp_token))`, where `wp_token` is the base64-encoded string of your username and password. The `image_path` should point to a valid image file on the local drive.
#'
#' @examples
#' \dontrun{
#' # Upload an image to the media library
#' wp_header <- httr::add_headers(Authorization = paste("Basic", wp_token))
#' upload_media(image_path = "path/to/image.jpg", wp_site = "https://example.com", wp_header = wp_header)
#' }
#'
#' @export
upload_media <- function(image_path, wp_site, wp_header) {
  # Prepare the API endpoint for media uploads
  api_url <- paste0(wp_site, "/wp-json/wp/v2/media")

  # Open the image file from the local drive
  image_file <- upload_file(image_path)

  # Set the filename in the Content-Disposition header
  filename <- basename(image_path)
  wp_header <- c(wp_header, add_headers(`Content-Disposition` = paste0('attachment; filename="', filename, '"')))

  # Send the POST request to upload the image
  response <- POST(api_url, wp_header, body = list(file = image_file), encode = "multipart")

  if (status_code(response) == 201) {  # Status 201 means created
    print("Image uploaded successfully.")
    return(content(response, "parsed", simplifyVector = TRUE))
  } else {
    stop(paste("Error uploading image:", status_code(response), content(response, "text")))
  }
}

#' Fetch WordPress Media Items
#'
#' Retrieves a list of media items from the WordPress media library using the REST API.
#'
#' @param wp_site Character. The base URL of the WordPress site (e.g., "https://example.com").
#' @param wp_header List. A named list created using `add_headers(Authorization = paste("Basic", wp_token))` for authentication, where `wp_token` is the base64-encoded username and password for the WordPress site.
#' @param per_page Numeric. The number of media items to retrieve per page (default is 10).
#' @param page Numeric. The page number to retrieve (default is 1).
#'
#' @return A list containing the media items retrieved for the specified page, or an error message if the request fails.
#'
#' @details This function allows users to fetch media items from the WordPress media library via the REST API. By specifying the `per_page` and `page` parameters, users can control how many media items to retrieve and from which page. If the `wp_header` is provided, the function will include authentication in the API request.
#'
#' @examples
#' \dontrun{
#' # Fetch the first page of media items with the default number per page
#' get_media(wp_site = "https://example.com", wp_header = wp_header)
#'
#' # Fetch the second page with 5 media items per page
#' get_media(wp_site = "https://example.com", wp_header = wp_header, per_page = 5, page = 2)
#' }
#'
#' @export
get_media <- function(wp_site, wp_header, per_page = 10, page = 1) {
  # Prepare the API endpoint for fetching media items
  api_url <- paste0(wp_site, "/wp-json/wp/v2/media?per_page=", per_page, "&page=", page)

  # Send the GET request to retrieve media items
  response <- GET(api_url, wp_header, content_type_json())

  if (status_code(response) == 200) {
    media_items <- content(response, "parsed", simplifyVector = TRUE)
    return(media_items)
  } else {
    stop(paste("Error fetching media items:", status_code(response), content(response, "text")))
  }
}

#' Delete Media Item from WordPress
#'
#' Deletes a specific media item from the WordPress media library using the REST API.
#'
#' @param media_id Numeric. The ID of the media item to be deleted.
#' @param wp_site Character. The base URL of the WordPress site (e.g., "https://example.com").
#' @param wp_header List. A named list created using `add_headers(Authorization = paste("Basic", wp_token))` for authentication, where `wp_token` is the base64-encoded username and password for the WordPress site.
#' @param force Logical. If `TRUE` (default), the media item will be permanently deleted. If `FALSE`, the media item will be moved to the trash.
#'
#' @return A list containing a confirmation message or an error message if the deletion fails.
#'
#' @details This function allows users to delete existing media items from the WordPress media library via the REST API. The `wp_header` parameter must be passed using `add_headers(Authorization = paste("Basic", wp_token))`, where `wp_token` is the base64-encoded string of your username and password. The `force` parameter controls whether the media item is permanently deleted or moved to the trash.
#'
#' @examples
#' \dontrun{
#' # Delete a media item by ID
#' wp_header <- httr::add_headers(Authorization = paste("Basic", wp_token))
#' delete_media(media_id = 12, wp_site = "https://example.com", wp_header = wp_header)
#'
#' # Move a media item to the trash
#' delete_media(media_id = 12, wp_site = "https://example.com", wp_header = wp_header, force = FALSE)
#' }
#'
#' @export
delete_media <- function(media_id, wp_site, wp_header, force = TRUE) {
  # Prepare the API endpoint for deleting media
  api_url <- paste0(wp_site, "/wp-json/wp/v2/media/", media_id, "?force=", tolower(as.character(force)))

  # Send the DELETE request to remove the media
  response <- DELETE(api_url, wp_header, content_type_json())

  if (status_code(response) == 200) {
    print("Media deleted successfully.")
    return(content(response, "parsed", simplifyVector = TRUE))
  } else {
    stop(paste("Error deleting media:", status_code(response), content(response, "text")))
  }
}
