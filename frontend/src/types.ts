export type LVZipper = [Env, LispVal, Array<LVCrumb>]

export type Env = Array<[string, LispVal]>

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

type Port = {
  type: 'port'
  value: string
}

type List<AdditionalInfo = {}> = {
  type: 'list'
  value: Array<LispVal<AdditionalInfo>>
}

type DottedList<AdditionalInfo = {}> = {
  type: 'dotted-list'
  head: Array<LispVal<AdditionalInfo>>
  tail: LispVal<AdditionalInfo>
}

type Func<AdditionalInfo = {}> = {
  type: 'func'
  params: Array<string>
  vararg?: string
  body: LispVal<AdditionalInfo>
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
  body: LispVal<AdditionalInfo>
  closure: Env
}

type LVCrumb = {
  type: 'crumb'
  env: Env
  ls: Array<LispVal>
  rs: Array<LispVal>
}

export type LispError = {
  error: string
}

export type LispVal<AdditionalInfo = {}> = (
  | Atom
  | Character
  | List<AdditionalInfo>
  | DottedList<AdditionalInfo>
  | Integer
  | Float
  | String
  | Bool
  | PrimitiveFunc
  | IOFunc
  | Port
  | Func<AdditionalInfo>
  | Macro<AdditionalInfo>
  | Unit
) &
  AdditionalInfo

export type LeftResult<T> = {
  Left?: T
}

export type RightResult<T> = {
  Right?: T
}

export type Result<E, T> = LeftResult<E> & RightResult<T>

export type FocusedLispVal = LispVal<{ focused: boolean }>

export type EvalRequest = {
  typeCheck: boolean
  macroExpand: boolean
  expression: string
}
