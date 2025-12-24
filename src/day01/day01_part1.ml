(* An example design that takes a series of input values and calculates the range between
  the largest and smallest one. *)


(* We generally open Core and Hardcaml in any source file in a hardware project. For
  design source files specifically, we also open Signal. *)
open! Core
open! Hardcaml
open! Signal


let num_bits = 16


(* Every hardcaml module should have an I and an O record, which define the module
  interface. *)
module I = struct
 type 'a t =
   { clock : 'a
   ; clear : 'a
   ; start : 'a
   ; finish : 'a
   ; data_in : 'a [@bits num_bits]
   ; data_in_valid : 'a
   }
 [@@deriving hardcaml]
end


module O = struct
 type 'a t =
   { (* With_valid.t is an Interface type that contains a [valid] and a [value] field. *)
     output : 'a With_valid.t [@bits num_bits]
   }
 [@@deriving hardcaml]
end


module States = struct
 type t =
   | Idle
   | Accepting_inputs
   | Done
 [@@deriving sexp_of, compare ~localize, enumerate]
end


let create scope ({ clock; clear; start; finish; data_in; data_in_valid } : _ I.t) : _ O.t
 =
 let spec = Reg_spec.create ~clock ~clear () in
 let open Always in
 let sm =
   (* Note that the state machine defaults to initializing to the first state *)
   State_machine.create (module States) spec
 in
 (* let%hw[_var] is a shorthand that automatically applies a name to the signal, which
    will show up in waveforms. The [_var] version is used when working with the Always
    DSL. *)
 let%hw_var cur_pos = Variable.reg spec ~width:num_bits in
 let%hw_var password = Variable.reg spec ~width:num_bits in
 let out_valid = Variable.wire ~default:gnd () in


 let cur_pos_val = cur_pos.value in
 let new_pos_val = (cur_pos_val +: data_in +: of_unsigned_int ~width:num_bits 1000) in (* Multiply by 1000 to make sure all positions values are positive for Barret Reduction*)
 let quotient = srl (new_pos_val *: (of_unsigned_int ~width:32 83887)) ~by:23 in (*Barret Reduction to take modulo 100 of answer using k = 23*)
 let est_remainder = (new_pos_val) -:  (uresize ~width:num_bits (quotient *: of_unsigned_int ~width:num_bits 100)) in
 let remainder = mux2 (est_remainder >: (of_unsigned_int ~width:num_bits 100)) (est_remainder-:of_unsigned_int ~width:num_bits 100) est_remainder in


 compile
   [ sm.switch
       [ ( Idle
         , [ when_
               start
               [ cur_pos <-- of_unsigned_int ~width:num_bits 50
               ; password <-- zero num_bits
               ; sm.set_next Accepting_inputs
               ]
           ] )
       ; ( Accepting_inputs
         , [ when_
               data_in_valid
               [ cur_pos <-- remainder
               ;  when_
                     (remainder ==: (of_unsigned_int ~width:num_bits 0))
                     [ password <-- password.value +: (of_unsigned_int ~width:num_bits 1) ]
               ]
           ; when_ finish [ sm.set_next Done ]
           ] )
       ; ( Done
         , [ out_valid <-- vdd
           ; when_ finish [ sm.set_next Accepting_inputs ]
           ] )
       ]
   ];
 (* [.value] is used to get the underlying Signal.t from a Variable.t in the Always DSL. *)
 { output = { value = password.value; valid = out_valid.value } }
;;


(* The [hierarchical] wrapper is used to maintain module hierarchy in the generated
  waveforms and (optionally) the generated RTL. *)
let hierarchical scope =
 let module Scoped = Hierarchy.In_scope (I) (O) in
 Scoped.hierarchical ~scope ~name:"day01_part1" create
;;
