(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Client = struct
  include Cohttp_lwt_unix.Client

  let ctx () =
    let resolver =
      let h = Hashtbl.create 1 in
      Hashtbl.add h "irmin" (`Unix_domain_socket "/var/run/irmin.sock");
      Resolver_lwt_unix.static h
    in
    Some (Cohttp_lwt_unix.Client.custom_ctx ~resolver ())
end

module Make = Irmin_http.Make (Client)
module KV (C : Irmin.Contents.S) =
  Make (Irmin.Metadata.None) (C) (Irmin.Path.String_list) (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
module Server = Irmin_http_server.Make (Cohttp_lwt_unix.Server)
