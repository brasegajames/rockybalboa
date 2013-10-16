(* Dessine un rectangle dans une matrice avec
les coordonnées n de la couleur color *)
let drawRect m n color =
  let (a, b, c, d) = n in
    let j = ref b in
      for i = a to c do
        m.(i).(!j) <- color;
      done;
      j := d;
      for i = a to c do
        m.(i).(!j) <- color;
      done;
      j := a;
      for i = b to d do
        m.(!j).(i) <- color;
      done;
      j := c;
      for i = b to d do
        m.(!j).(i) <- color;
      done
