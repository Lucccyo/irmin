(*
   Copyright (c) 2016 David Kaloper Meršinjak
   Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

(* Extracted from https://github.com/pqwy/lru *)

module MakeUnsafe (H : Hashtbl.HashedType) = struct
  open Kcas

  module Q = struct
    type 'a node = {
      value : 'a;
      next : 'a node option Loc.t;
      prev : 'a node option Loc.t;
    }

    type 'a t = { first : 'a node option Loc.t; last : 'a node option Loc.t }

    let detach ~xt t n =
      let np = Xt.get ~xt n.prev and nn = Xt.get ~xt n.next in
      (match np with
      | None -> Xt.set ~xt t.first nn
      | Some x ->
          Xt.set ~xt x.next nn;
          Xt.set ~xt n.prev None);
      match nn with
      | None -> Xt.set ~xt t.last np
      | Some x ->
          Xt.set ~xt x.prev np;
          Xt.set ~xt n.next None

    let append ~xt t n =
      let on = Some n in
      match Xt.get ~xt t.last with
      | Some x as l ->
          Xt.set ~xt x.next on;
          Xt.set ~xt t.last on;
          Xt.set ~xt n.prev l
      | None ->
          Xt.set ~xt t.first on;
          Xt.set ~xt t.last on

    let node x = { value = x; prev = Loc.make None; next = Loc.make None }
    let create () = { first = Loc.make None; last = Loc.make None }

    let iter ~xt t f =
      let rec aux f = function
        | Some n ->
            let next = Xt.get ~xt n.next in
            f n.value;
            aux f next
        | _ -> ()
      in
      aux f (Xt.get ~xt t.first)

    let clear ~xt t =
      Xt.set ~xt t.first None;
      Xt.set ~xt t.last None
  end

  type key = H.t

  type 'a t = {
    ht : (key, (key * 'a) Q.node) Kcas_data.Hashtbl.t;
    q : (key * 'a) Q.t;
    cap : cap Loc.t;
    w : int Loc.t;
  }

  and cap = Uncapped | Capped of int

  let weight ~xt t = Xt.get ~xt t.w

  let create cap =
    let cap, ht_cap =
      if cap < 0 then (Uncapped, 65536) else (Capped cap, cap)
    in
    {
      cap = Loc.make cap;
      w = Loc.make 0;
      ht =
        Kcas_data.Hashtbl.create ~min_buckets:ht_cap ~hashed_type:(module H) ();
      q = Q.create ();
    }

  let drop ~xt t =
    match Xt.get ~xt t.q.first with
    | None -> None
    | Some ({ Q.value = k, v; _ } as n) ->
        Xt.decr ~xt t.w;
        Kcas_data.Hashtbl.Xt.remove ~xt t.ht k;
        Q.detach ~xt t.q n;
        Some v

  let remove ~xt t k =
    match Kcas_data.Hashtbl.Xt.find_opt ~xt t.ht k with
    | Some n ->
        Xt.decr ~xt t.w;
        Kcas_data.Hashtbl.Xt.remove ~xt t.ht k;
        Q.detach ~xt t.q n
    | None -> ()

  let add ~xt t k v =
    let add t k v =
      remove ~xt t k;
      let n = Q.node (k, v) in
      Xt.incr ~xt t.w;
      Kcas_data.Hashtbl.Xt.add ~xt t.ht k n;
      Q.append ~xt t.q n
    in
    match Xt.get ~xt t.cap with
    | Capped c when c = 0 -> ()
    | Uncapped -> add t k v
    | Capped c ->
        add t k v;
        if weight ~xt t > c then
          let _ = drop ~xt t in
          ()

  let promote ~xt t k =
    match Kcas_data.Hashtbl.Xt.find_opt ~xt t.ht k with
    | Some n ->
        Q.(
          detach ~xt t.q n;
          append ~xt t.q n)
    | None -> ()

  let find_opt ~xt t k =
    match Kcas_data.Hashtbl.Xt.find_opt ~xt t.ht k with
    | Some v ->
        promote ~xt t k;
        Some (snd v.value)
    | None -> None

  let mem ~xt t k =
    match Kcas_data.Hashtbl.Xt.mem ~xt t.ht k with
    | false -> false
    | true ->
        promote ~xt t k;
        true

  let iter ~xt t f = Q.iter ~xt t.q (fun (k, v) -> f k v)

  let clear ~xt t =
    Loc.set t.w 0;
    Kcas_data.Hashtbl.Xt.clear ~xt t.ht;
    Q.clear ~xt t.q
end

module Make (H : Hashtbl.HashedType) = struct
  module Unsafe = MakeUnsafe (H)

  type 'a t = 'a Unsafe.t

  let create cap = Unsafe.create cap
  let add data k v = Kcas.Xt.commit { tx = Unsafe.add data k v }
  let find_opt data k = Kcas.Xt.commit { tx = Unsafe.find_opt data k }
  let find t k = match find_opt t k with Some v -> v | None -> raise Not_found
  let mem data k = Kcas.Xt.commit { tx = Unsafe.mem data k }
  let iter data f = Kcas.Xt.commit { tx = Unsafe.iter data f }
  let clear data = Kcas.Xt.commit { tx = Unsafe.clear data }
  let drop data = Kcas.Xt.commit { tx = Unsafe.drop data }
end
