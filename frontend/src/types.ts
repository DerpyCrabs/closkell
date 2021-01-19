export type ValueZipper = [Env, Value, Array<ValueCrumb>]

export type Env = Array<[string, Value]>

type String = {
  type: 'string'
  value: string
}

type Character = {
  type: 'character'
  value: string
}

type Atom = {
  type: 'atom'
  value: string
}

type Integer = {
  type: 'integer'
  value: number
}

type Float = {
  type: 'float'
  value: number
}

type Bool = {
  type: 'bool'
  value: boolean
}

type List<AdditionalInfo = {}> = {
  type: 'list'
  value: Array<Value<AdditionalInfo>>
}

type Map<AdditionalInfo = {}> = {
  type: 'map'
  value: Array<Value<AdditionalInfo>>
}

type Call<AdditionalInfo = {}> = {
  type: 'call'
  value: Array<Value<AdditionalInfo>>
}

type DottedList<AdditionalInfo = {}> = {
  type: 'dotted-list'
  head: Array<Value<AdditionalInfo>>
  tail: Value<AdditionalInfo>
}

type Func<AdditionalInfo = {}> = {
  type: 'func'
  params: Array<string>
  vararg?: string
  body: Value<AdditionalInfo>
  closure: Env
}

type PrimitiveFunc = {
  type: 'primitive-func'
  value: string
}

type IOFunc = {
  type: 'io-func'
  value: string
}

type Unit = {
  type: 'unit'
}

type Macro<AdditionalInfo = {}> = {
  type: 'macro'
  body: Value<AdditionalInfo>
  closure: Env
}

type ValueCrumb = {
  type: 'crumb'
  env: Env
  ls: Array<Value>
  rs: Array<Value>
}

export type Error = {
  error: string
}

export type Value<AdditionalInfo = {}> = (
  | Atom
  | Character
  | List<AdditionalInfo>
  | Map<AdditionalInfo>
  | Call<AdditionalInfo>
  | DottedList<AdditionalInfo>
  | Integer
  | Float
  | String
  | Bool
  | PrimitiveFunc
  | IOFunc
  | Func<AdditionalInfo>
  | Macro<AdditionalInfo>
  | Unit
) &
  AdditionalInfo

export type FocusedValPath = Array<number>

export type FocusedLispVal = [Value, FocusedValPath | null]

export type LeftResult<T> = {
  Left?: T
}

export type RightResult<T> = {
  Right?: T
}

export type Result<E, T> = LeftResult<E> & RightResult<T>

export type EvalRequest = {
  typeCheck: boolean
  macroExpand: boolean
  expression: string
}
