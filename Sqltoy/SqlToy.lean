
import Batteries.Data.Vector.Basic
import Batteries.Data.HashMap.Basic
import Batteries.Data.HashMap.Lemmas
import Batteries.Data.Vector.Lemmas
--import Init.Data.Range
import Batteries.Data.List.Lemmas



--import Mathlib.Tactic.SolveByElim
import Init.Control.State

set_option linter.unusedVariables false
--set_option diagnostics true

namespace sql
open Batteries Vector HashMap

inductive RowType where
  | RowStr : String → RowType
  | RowInt : Int → RowType
  deriving Repr
open RowType

#check [RowStr _, RowInt _]

instance : ToString RowType where
  toString rt := match rt with
    | RowStr s => s
    | RowInt n => toString n

abbrev Row := List RowType

instance : ToString Row where
  toString xs := Id.run do
    let mut s := "\n["
    for x in xs do
      s := s ++ (toString x) ++ ", "
    return s ++ "]"

abbrev Rows := List Row

structure Table where
  name: String
  rows: List Row
  deriving Repr

instance : ToString Table where
  toString t := Id.run do
    let mut s := s!"[Table {t.name}"
    for r in t.rows do
      s := s ++ (toString r)
    return s ++ "]"


structure SqlState where
  tables : HashMap String Table

instance : ToString SqlState where
  toString (ss) :=
    let num_tables := ss.tables.size
    s!"[Tables: {ss.tables.toList}]"

abbrev Sql := StateT SqlState IO

#check Sql

def init_sql_toy: SqlState := SqlState.mk HashMap.empty

def get_table (table_name: String) : Sql (Option Table) := do --: Sql Table := do
  let db ← get
  return db.tables.find? table_name

def set_table (name: String) (table: Table) : Sql Unit := do
  -- replace table in database
  let db ← get
  let ts := db.tables.insert name table
  set (SqlState.mk ts)

def CREATE_TABLE (name: String) : Sql Unit  := do
  -- add table to database
  let t := Table.mk name []
  set_table name t

def get_rows (table_name: String) : Sql (Option Rows) := do
  let t ← get_table table_name
  match t with
  | some t => return t.rows
  | none => do
      println! s!"Could not find table with name `{table_name}`"
      return none

def set_rows (table_name: String) (rows: Rows) : Sql Unit := do
  let mut t ← get_table table_name
  match t with
  | some table => set_table table_name {table with rows := rows}
  | none => do
      println! s!"Could not find table with name {table_name}"
      return

def INSERT_INTO := set_rows
def FROM := get_table

def CROSS_JOIN (name₁ name₂ : String) : Sql Table := do
  let t₁ ← get_table name₁
  let t₂ ← get_table name₂

  match t₁ with
  | some tab₁ =>
    match t₂ with
    | some tab₂ => do
      let rows := List.zipWith List.append tab₁.rows tab₂.rows
      return (Table.mk "hum" rows)
    | none => return (Table.mk "error" [])
  | none => return (Table.mk "error" [])

def start := do

  CREATE_TABLE "left"
  CREATE_TABLE "right"

  INSERT_INTO "left" [
    [RowInt 1, RowStr "Left1"],
    [RowInt 2, RowStr "Left2"]
  ]

  INSERT_INTO "right" [
    [RowInt 1, RowStr "Right1"],
    [RowInt 2, RowStr "Right2"]
  ]

  let t₃ ← CROSS_JOIN "left" "right"
  println! t₃

def main : IO Unit := do
  let a ← (StateT.run start init_sql_toy)

#eval main

end sql
