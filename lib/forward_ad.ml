type dual = { primal : float; tangent : float }

let make primal tangent = { primal; tangent }
let constant x = make x 0.0
let variable x = make x 1.0
let value d = d.primal
let derivative d = d.tangent

let add left right =
  make (left.primal +. right.primal) (left.tangent +. right.tangent)

let sub left right =
  make (left.primal -. right.primal) (left.tangent -. right.tangent)

let mul left right =
  let primal = left.primal *. right.primal in
  let tangent =
    (left.tangent *. right.primal) +. (left.primal *. right.tangent)
  in
  make primal tangent

let div left right =
  if right.primal = 0.0 then failwith "division by zero in forward_ad.div"
  else
    let primal = left.primal /. right.primal in
    let numerator =
      (left.tangent *. right.primal) -. (left.primal *. right.tangent)
    in
    let denominator = right.primal *. right.primal in
    make primal (numerator /. denominator)

let neg inner = make (-.inner.primal) (-.inner.tangent)

let exp inner =
  let primal = Stdlib.exp inner.primal in
  make primal (primal *. inner.tangent)

let log inner =
  if inner.primal <= 0.0 then failwith "log domain error in forward_ad.log"
  else make (Stdlib.log inner.primal) (inner.tangent /. inner.primal)

let sin inner =
  make (Stdlib.sin inner.primal) (Stdlib.cos inner.primal *. inner.tangent)

let cos inner =
  make (Stdlib.cos inner.primal) (-.Stdlib.sin inner.primal *. inner.tangent)

let tanh inner =
  let primal = Stdlib.tanh inner.primal in
  make primal ((1.0 -. (primal *. primal)) *. inner.tangent)

let derivative_univariate f x =
  let output = f (variable x) in
  output.tangent

let rec eval_dual expr env =
  match expr with
  | Expr.Const c -> constant c
  | Expr.Var v -> (
      try List.assoc v env
      with Not_found -> failwith ("missing variable assignment for " ^ v))
  | Expr.Add (e1, e2) ->
      let d1 = eval_dual e1 env in
      let d2 = eval_dual e2 env in
      add d1 d2
  | Expr.Sub (e1, e2) ->
      let d1 = eval_dual e1 env in
      let d2 = eval_dual e2 env in
      sub d1 d2
  | Expr.Mul (e1, e2) ->
      let d1 = eval_dual e1 env in
      let d2 = eval_dual e2 env in
      mul d1 d2
  | Expr.Div (e1, e2) ->
      let d1 = eval_dual e1 env in
      let d2 = eval_dual e2 env in
      div d1 d2
  | Expr.Neg e -> neg (eval_dual e env)
  | Expr.Exp e -> exp (eval_dual e env)
  | Expr.Log e -> log (eval_dual e env)
  | Expr.Sin e -> sin (eval_dual e env)
  | Expr.Cos e -> cos (eval_dual e env)
  | Expr.Tanh e -> tanh (eval_dual e env)

let gradient_expr expr env =
  let vars = Expr.vars expr in
  try
    let primal =
      let base_env =
        List.map (fun (name, value) -> (name, constant value)) env
      in
      value (eval_dual expr base_env)
    in
    let grads =
      List.map
        (fun var ->
          let dual_env =
            List.map
              (fun (name, v) ->
                if name = var then (name, variable v) else (name, constant v))
              env
          in
          let output = eval_dual expr dual_env in
          (var, derivative output))
        vars
    in
    Ok (primal, grads)
  with Failure msg ->
    if msg = "division by zero in forward_ad.div" then Error "division by zero"
    else if msg = "log domain error in forward_ad.log" then
      Error "log undefined for non-positive input"
    else Error msg
