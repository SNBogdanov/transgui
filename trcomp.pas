

{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

Unit trcomp;

Interface

Uses 
VarGrid, LazarusPackageIntf;

Implementation

Procedure Register;
Begin
  RegisterUnit('VarGrid', @VarGrid.Register);
End;

initialization
RegisterPackage('trcomp', @Register);
End.
