type types = 
  | Int
  | Bool
  | Fleche of types * types
  | X of int
  | Etoile of types * types
  | List of types

type schematype = 
  | Type of types
  | Forall of types * schematype 
