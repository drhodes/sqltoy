import Lake
open Lake DSL

package «sqltoy» where
  -- add package configuration options here

lean_lib «Sqltoy» where
  -- add library configuration options here

@[default_target]
lean_exe «sqltoy» where
  root := `Main

require batteries from git "https://github.com/leanprover-community/batteries" @ "main"
