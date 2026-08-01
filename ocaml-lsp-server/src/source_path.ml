open Import
module Misc = Ocaml_utils.Misc

let canonicalize path =
  let path = Misc.canonicalize_filename path in
  let rec resolve_existing_parent path missing =
    match Unix.realpath path with
    | resolved -> List.fold_left missing ~init:resolved ~f:Filename.concat
    | exception Unix.Unix_error ((Unix.ENOENT | ENOTDIR), _, _) ->
      let parent = Filename.dirname path in
      if String.equal parent path
      then path
      else resolve_existing_parent parent (Filename.basename path :: missing)
    | exception Unix.Unix_error _ -> path
  in
  resolve_existing_parent path []
;;

let of_path path =
  if Filename.is_relative path then Uri.of_path path else Uri.of_path (canonicalize path)
;;

let uri uri =
  let path = Uri.to_path uri in
  if Filename.is_relative path && not (Sys.file_exists path) then uri else of_path path
;;

let location ({ Location.uri = source_uri; range } : Location.t) =
  { Location.uri = uri source_uri; range }
;;

let deduplicate_locations locations =
  let _, locations =
    List.fold_left
      locations
      ~init:([], [])
      ~f:(fun (seen, locations) ({ Location.uri; range } as location) ->
        let key = uri, range in
        if List.exists seen ~f:(Poly.equal key)
        then seen, locations
        else key :: seen, location :: locations)
  in
  List.rev locations
;;
