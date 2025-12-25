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

 let%hw new_pos_val = (cur_pos.value +: data_in +: of_unsigned_int ~width:num_bits 1000) in (* Add 1000 to make sure all positions values are positive for Barret Reduction*)
 let%hw est_quotient = srl (new_pos_val *: (of_unsigned_int ~width:32 83887)) ~by:23 in (*Barret Reduction to take modulo 100 of answer using k = 23*)
 let%hw est_remainder = new_pos_val -: uresize ~width:num_bits (est_quotient *: of_unsigned_int ~width:num_bits 100) in

 let%hw need_correction = est_remainder >: (of_unsigned_int ~width:num_bits 99) in (*Accounts for if the reduction was off-by-one*)
 
 let%hw corrected_remainder = mux2 need_correction 
      (est_remainder -: of_unsigned_int ~width:num_bits 100) 
      est_remainder 
 in

 let%hw corrected_quotient = mux2 need_correction 
      ((uresize ~width:num_bits est_quotient) -: of_unsigned_int ~width:num_bits 11) 
      (uresize ~width:num_bits est_quotient -: of_unsigned_int ~width:num_bits 10) 
 in

 let%hw pos_quotient = mux2 (corrected_quotient <+ (of_unsigned_int ~width:num_bits 0))
      (uresize ~width:num_bits (corrected_quotient *+ of_signed_int ~width:num_bits (-1))) 
      corrected_quotient 
 in

 let%hw at_zero = corrected_remainder ==: of_unsigned_int ~width:num_bits 0 in

 let%hw corrective_factor1 = mux2 (at_zero &: (corrected_quotient >: of_unsigned_int ~width:num_bits 0)) (*Accounts for double count of going right to zero*)
      (of_signed_int ~width:num_bits (-1))
      (of_unsigned_int ~width:num_bits 0)
 in

 let%hw corrective_factor2 = mux2 ((cur_pos.value ==: of_unsigned_int ~width:num_bits 0) &: (corrected_quotient <+ of_unsigned_int ~width:num_bits 0)) (*Accounts for double count of going left from zero*)
      (of_signed_int ~width:num_bits (-1))
      (of_unsigned_int ~width:num_bits 0)
 in

 let%hw final_shift = pos_quotient +: corrective_factor1 +: corrective_factor2 in

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
               [ cur_pos <-- corrected_remainder
               ;  if_
                     (at_zero)
                     [ password <-- password.value +: final_shift +: (of_unsigned_int ~width:num_bits 1) ]
                     [ password <-- password.value +: final_shift ]
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
