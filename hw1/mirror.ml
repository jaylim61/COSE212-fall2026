exception NotImplemented;;

type btree =
| Leaf of int
| Left of btree
| Right of btree
| LeftRight of btree * btree

let mirror: btree -> btree
=fun b -> raise NotImplemented;; (* TODO *)