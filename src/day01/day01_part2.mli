open! Core
open! Hardcaml


val num_bits : int


module I : sig
 type 'a t =
   { clock : 'a
   ; clear : 'a
   ; start : 'a
   ; finish : 'a
   ; data_in : 'a
   ; data_in_valid : 'a
   }
 [@@deriving hardcaml]
end


module O : sig
 type 'a t = { output : 'a With_valid.t } [@@deriving hardcaml]
end


val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t