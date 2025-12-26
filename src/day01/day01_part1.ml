open! Core
open! Hardcaml
open! Signal


let num_bits = 16

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
   {
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
   State_machine.create (module States) spec
 in
 let%hw_var cur_pos = Variable.reg spec ~width:num_bits in
 let%hw_var password = Variable.reg spec ~width:num_bits in
 let out_valid = Variable.wire ~default:gnd () in


 let div_100 x = srl (x *: (of_unsigned_int ~width:32 83887)) ~by:23 in (*Barret Reduction to divide by 100 using k = 23*)
 let num x = of_signed_int ~width:num_bits x in

 let%hw new_pos_val = (cur_pos.value +: data_in +: num 1000) in (* Add 1000 to make sure all positions values are positive for Barret Reduction*)
 let%hw quotient = div_100 new_pos_val in 
 let%hw remainder = new_pos_val -: uresize ~width:num_bits (quotient *: num 100) in

 compile
   [ sm.switch
       [ ( Idle
         , [ when_
               start
               [ cur_pos <-- num 50
               ; password <-- zero num_bits
               ; sm.set_next Accepting_inputs
               ]
           ] )
       ; ( Accepting_inputs
         , [ when_
               data_in_valid
               [ cur_pos <-- remainder
               ;  when_
                     (remainder ==: (num 0))
                     [ password <-- password.value +: (num 1) ]
               ]
           ; when_ finish [ sm.set_next Done ]
           ] )
       ; ( Done
         , [ out_valid <-- vdd
           ; when_ finish [ sm.set_next Accepting_inputs ]
           ] )
       ]
   ];
 { output = { value = password.value; valid = out_valid.value } }
;;

let hierarchical scope =
 let module Scoped = Hierarchy.In_scope (I) (O) in
 Scoped.hierarchical ~scope ~name:"day01_part1" create
;;
