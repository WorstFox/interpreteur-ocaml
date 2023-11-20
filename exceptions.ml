(* définition exceptions *)
exception NotInt
exception NotBool
exception NotConsistant
exception NotExpected
exception NotVariable
exception NotFunction
exception NotRecursiveFunction
exception MissingVariable of string
exception TooMuchVariable
exception StackOverflow
exception NotReference
exception PasBienDef
exception NotList
exception NotMatched
exception NotCase
exception NotUnifyable

exception E of int (* uniquement dans l'écriture fouine *)
