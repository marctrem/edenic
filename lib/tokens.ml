

type member_constness =
  | Const
  | Mut

type field = 
  {
    name: string;
    constness: member_constness;
    type_: string list;
  }

type model_component = [
  | `Field of field
  | `Prefix of string
  | `Store
  | `ReservedSerials of int list
]

type model = {
    name: string;
    components: model_component list (* alist *)
}

type prog = [
  | `Model of model
]
