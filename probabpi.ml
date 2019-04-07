type point2d = {x : float; y : float}

(* vector magnitude *)
let magnitude {x = xcoord; y = ycoord} =
  sqrt (xcoord ** 2. +. ycoord ** 2.)

(* Cartesian distance between two points *)
let distance v1 v2 =
    magnitude {x = v1.x -. v2.x; y = v1.y -. v2.y}

(* generate a point inside a square with a given side *)
let gen side =
  {x = Random.float side; y = Random.float side}

(* if point is inside circle, return 1,
 * otherwise 0 *)
let in_circle side center radius =
  let point = gen side in
  if distance point center < radius then 1
  else 0

let rec num_in_circle runs side center r accum =
  match runs with
  | 0 -> accum
  | _ -> num_in_circle (runs - 1) 
                       side 
                       center 
                       r 
                       (accum + in_circle side center r)

let () =
  let runs = int_of_string Sys.argv.(1) in
  Random.self_init ();

(* generate points inside of a unit square,
 * and count how many are also inside of a circle
 * with a radius of 1/2 and 
 * a center coinsiding with the center of a square *)
  let side = 1.0 in
  let center = {x=0.5; y=0.5} in
  let r = 0.5 in

  let num_in = num_in_circle runs side center r 0 in

(* the area of the square A_s is side*side,
 * the area of the inscribed circle A_c is pi*side*side/4 or pi*r*r
 * therefore 4*A_c/A_s = pi
 * since the number of points inside the circle is proportional to the area
 * of the circle due to the uniform distribution of the coordinates,
 * the ratio of the number of points inside the circle to the total number of 
 * points converges to the ratio of the corresponding areas as the number of 
 * runs approaches infinity.
 *)
  let pi = 4. *. (float_of_int num_in) /. (float_of_int runs) in

  Printf.printf "%f\n" pi
