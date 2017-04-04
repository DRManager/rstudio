env <- function(...) {
   list2env(list(...))
}

object <- list(
   a = 1:100,
   b = list(
      b1 = "b1",
      b2 = list(
         b21 = "b21",
         b22 = "b22"
      )
   ),
   c = env(
      c1 = "c1",
      c2 = "c2"
   )
)

# debug(.rs.rpc.explorer_inspect_object)
context <- .rs.explorer.createContext(recursive = 1)
i <- .rs.explorer.inspectObject(object, context)
.rs.explorer.viewObject(object)
