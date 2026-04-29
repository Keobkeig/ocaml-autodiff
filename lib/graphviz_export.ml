type dot_graph_builder = {
  mutable counter : int;
  mutable nodes : (int * string) list;
  mutable edges : (int * int) list;
  vars : (string, int) Hashtbl.t;
}

let make_graph () =
  { counter = 0; nodes = []; edges = []; vars = Hashtbl.create 16 }

let generate_id builder =
  let id = builder.counter in
  builder.counter <- builder.counter + 1 ;
  id

let rec to_graph builder expr =
  let open Expr in
  match expr with
  | Const c ->
      let id = generate_id builder in
      builder.nodes <- (id, string_of_float c) :: builder.nodes ;
      id
  | Var s -> (
      match Hashtbl.find_opt builder.vars s with
      | Some id -> id
      | None ->
          let id = generate_id builder in
          builder.nodes <- (id, s) :: builder.nodes ;
          Hashtbl.add builder.vars s id ;
          id)
  | Add (l, r) -> to_graph_binary builder l r "+"
  | Sub (l, r) -> to_graph_binary builder l r "-"
  | Mul (l, r) -> to_graph_binary builder l r "*"
  | Div (l, r) -> to_graph_binary builder l r "/"
  | Neg e -> to_graph_unary builder e "-"
  | Exp e -> to_graph_unary builder e "exp"
  | Log e -> to_graph_unary builder e "log"
  | Sin e -> to_graph_unary builder e "sin"
  | Cos e -> to_graph_unary builder e "cos"
  | Tanh e -> to_graph_unary builder e "tanh"

and to_graph_binary builder l r symbol =
  let id = generate_id builder in
  builder.nodes <- (id, symbol) :: builder.nodes ;
  let l_id = to_graph builder l in
  let r_id = to_graph builder r in
  builder.edges <- (l_id, id) :: (r_id, id) :: builder.edges ;
  id

and to_graph_unary builder e symbol =
  let id = generate_id builder in
  builder.nodes <- (id, symbol) :: builder.nodes ;
  let e_id = to_graph builder e in
  builder.edges <- (e_id, id) :: builder.edges ;
  id

let expr_to_dot _expr =
  let builder = make_graph () in
  let _ = to_graph builder _expr in
  let buf = Buffer.create 64 in
  Buffer.add_string buf
    "digraph {\n\
    \  rankdir=\"LR\"\n\
    \  graph [pad=\"0.4\", nodesep=\"0.5\", ranksep=\"1.\",overlap=false, \
     splines = true]\n\
    \  node [shape=circle, fixedsize = true, penwidth = 0.5, height =0.3, \
     width =0.3, fontsize=11, color=orange, fontcolor=white]\n\
    \  edge [color=white, arrowsize=0.2]\n\
    \  bgcolor=\"black\" \n" ;
  List.iter
    (fun (id, label) ->
      Buffer.add_string buf (Printf.sprintf "  %d [label=\"%s\"]\n" id label))
    builder.nodes ;
  List.iter
    (fun (src, dst) ->
      Buffer.add_string buf (Printf.sprintf "  %d -> %d\n" src dst))
    builder.edges ;
  Buffer.add_string buf "}\n" ;
  Buffer.contents buf

let write_expr_dot path expr =
  let dot = expr_to_dot expr in
  try
    let channel = open_out path in
    output_string channel dot ;
    close_out channel ;
    Ok ()
  with Sys_error message -> Error message
