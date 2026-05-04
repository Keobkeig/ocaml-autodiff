(** Numerical gradient checking scaffolding for autodiff modules. *)

val central_difference : (float -> float) -> float -> float -> float
(** [central_difference f x eps] computes a central-difference estimate of
    [f'(x)] using step size [eps]. Preconditions: [eps > 0.0]. *)

val gradient_error : abs_tol:float -> expected:float -> actual:float -> bool
(** [gradient_error abs_tol expected actual] checks whether two gradients are
    within absolute tolerance [abs_tol]. *)

val check_expr_gradient :
  Expr.t ->
  (string * float) list ->
  string ->
  float ->
  float ->
  (bool, string) result
(** [check_expr_gradient expr env var_name eps abs_tol] compares reverse-mode
    gradients against numerical finite differences for [var_name]. Returns [true]
    if the gradients match within [abs_tol], or an error on failure. *)
