'library.protolite' <- function() {

   # load protolite library with mvt_sf_linestring patch
   # Awaiting fix to Protolite
   #' @import protolite
   #' @importFrom utils assignInNamespace



   mvt_sf_linestring <- function(mat){

      if (protolite:::all_equal(mat[, 3])) {
         sf::st_linestring(mat[, 1:2, drop = FALSE])    # added drop = FALSE
      }
      else {
         sf::st_multilinestring(protolite:::split_matrix_groups(mat))
      }
   }
   assignInNamespace("mvt_sf_linestring",mvt_sf_linestring,ns="protolite")
}
