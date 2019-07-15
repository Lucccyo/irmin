module Make (K : Irmin.Hash.S) = struct
  module Key = struct
    type t = K.t

    let pp = Irmin.Type.pp K.t

    let hash = K.short_hash

    let equal = Irmin.Type.equal K.t

    let encode = Irmin.Type.to_bin_string K.t

    let decode s off = snd (Irmin.Type.decode_bin K.t s off)

    let encoded_size = K.hash_size
  end

  module Val = struct
    type t = int64 * int * char

    let pp = Irmin.Type.(pp (triple int64 int char))

    let encode (off, len, kind) =
      Irmin.Type.(to_bin_string (triple int64 int32 char))
        (off, Int32.of_int len, kind)

    let decode s off =
      let off, len, kind =
        snd (Irmin.Type.(decode_bin (triple int64 int32 char)) s off)
      in
      (off, Int32.to_int len, kind)

    let encoded_size = (64 / 8) + (32 / 8) + 1
  end

  include Index_unix.Make (Key) (Val)
end
