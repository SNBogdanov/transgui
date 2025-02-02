

{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2019 by Yury Sidorov and Transmission Remote GUI working group.

  Transmission Remote GUI is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  Transmission Remote GUI is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Transmission Remote GUI; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

  In addition, as a special exception, the copyright holders give permission to 
  link the code of portions of this program with the
  OpenSSL library under certain conditions as described in each individual
  source file, and distribute linked combinations including the two.

  You must obey the GNU General Public License in all respects for all of the
  code used other than OpenSSL.  If you modify file(s) with this exception, you
  may extend this exception to your version of the file(s), but you are not
  obligated to do so.  If you do not wish to do so, delete this exception
  statement from your version.  If you delete this exception statement from all
  source files in the program, then also delete it here.
*************************************************************************************}

Unit BaseForm;

{$mode objfpc}

Interface

Uses 
Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics;

Type 

  { TBaseForm }

  TBaseForm = Class(TForm)
    Private 
      FNeedAutoSize: boolean;
      Procedure DoScale(C: TControl);
      Procedure InitScale;
    Protected 
      Procedure DoCreate;
      override;
    Public 
      constructor Create(TheOwner: TComponent);
      override;
  End;

Procedure AutoSizeForm(Form: TCustomForm);
Function ScaleInt(i: integer): integer;

Var 
  IntfScale: integer = 100;

Implementation

Uses LCLType, ButtonPanel, VarGrid, ComCtrls, StdCtrls, ExtCtrls, lclversion;

Var 
  ScaleMultiplier, ScaleDivider: integer;

Function ScaleInt(i: integer): integer;
Begin
  Result := i*ScaleMultiplier Div ScaleDivider;
End;

Type THackControl = Class(TWinControl)
End;

Procedure AutoSizeForm(Form: TCustomForm);

Var 
  i, ht, w, h: integer;
  C: TControl;
Begin
  ht := 0;
  For i:=0 To Form.ControlCount - 1 Do
    Begin
      C := Form.Controls[i];
      If Not C.Visible Then
        continue;
      With C Do
        Begin
          If C is TButtonPanel Then
            Begin
              TButtonPanel(C).HandleNeeded;
              w := 0;
              h := 0;
              THackControl(C).CalculatePreferredSize(w, h, True);
            End
          Else
            h := Height;
{$ifdef LCLcarbon}
          If C is TPageControl Then
            Inc(h, ScaleInt(10));
{$endif LCLcarbon}
          Inc(ht, h + BorderSpacing.Top + BorderSpacing.Bottom + BorderSpacing.
              Around*2);
        End;
    End;
  ht := ht + 2*Form.BorderWidth;

  Form.ClientHeight := ht;
  If Form.ClientHeight <> ht Then
    Begin
      Form.Constraints.MinHeight := 0;
      Form.ClientHeight := ht;
      Form.Constraints.MinHeight := Form.Height;
    End;
  If Form.BorderStyle = bsDialog Then
    Begin
      Form.Constraints.MinHeight := Form.Height;
      Form.Constraints.MinWidth := Form.Width;
    End;
End;

{ TBaseForm }

Procedure TBaseForm.DoScale(C: TControl);

Var 
  i: integer;
  R: TRect;
  w, h: integer;
Begin
  With C Do
    Begin
{$ifdef darwin}
      If C is TButtonPanel Then
        exit;
{$endif darwin}
      If C is TWinControl Then
        TWinControl(C).DisableAlign;
      Try
        If ScaleMultiplier <> ScaleDivider Then
          Begin
            ScaleConstraints(ScaleMultiplier, ScaleDivider);
            R := BaseBounds;
            R.Left := ScaleInt(R.Left);
            R.Top := ScaleInt(R.Top);
            R.Right := ScaleInt(R.Right);
            R.Bottom := ScaleInt(R.Bottom);
            If (Parent <> Nil) And (Align = alNone) Then
              Begin
                If akRight In Anchors Then
                  Inc(R.Right, C.Parent.ClientWidth - ScaleInt(C.
                      BaseParentClientSize.cx));
                If akBottom In Anchors Then
                  Inc(R.Bottom, C.Parent.ClientHeight - ScaleInt(C.
                      BaseParentClientSize.cy));
              End;
            BoundsRect := R;
            With BorderSpacing Do
              Begin
                Top := ScaleInt(Top);
                Left := ScaleInt(Left);
                Bottom := ScaleInt(Bottom);
                Right := ScaleInt(Right);
                Around := ScaleInt(Around);
                InnerBorder := ScaleInt(InnerBorder);
              End;

            If C is TWinControl Then
              With TWinControl(C).ChildSizing Do
                Begin
                  HorizontalSpacing := ScaleInt(HorizontalSpacing);
                  VerticalSpacing := ScaleInt(VerticalSpacing);
                  LeftRightSpacing := ScaleInt(LeftRightSpacing);
                  TopBottomSpacing := ScaleInt(TopBottomSpacing);
                End;

            If C is TButtonPanel Then
              TButtonPanel(C).Spacing := ScaleInt(TButtonPanel(C).Spacing);

            If C is TVarGrid Then
              With TVarGrid(C).Columns Do
                For i:=0 To Count - 1 Do
                  Items[i].Width := ScaleInt(Items[i].Width);
            If C is TStatusBar Then
              With TStatusBar(C) Do
                For i:=0 To Panels.Count - 1 Do
                  Panels[i].Width := ScaleInt(Panels[i].Width);
          End;

        // Runtime fixes

        // Fix right aligned label autosize
        If C.Visible And (C is TCustomLabel) And C.AutoSize And (TLabel(C).
           Alignment = taLeftJustify) And (C.Anchors*[akLeft, akRight] = [
           akRight]) Then
          Begin
            w := 0;
            h := 0;
            THackControl(C).CalculatePreferredSize(w, h, True);
            C.Width := w;
          End;
{$ifdef darwin}
        // Always use standard button height on OS X for proper theming
        If C.Visible And (C is TCustomButton) Then
          Begin
            w := 0;
            h := 0;
            THackControl(C).CalculatePreferredSize(w, h, True);
            C.Height := h;
          End;
        // Add extra top spacing for group box
        i := ScaleInt(6);
        If C.Parent is TCustomGroupBox Then
          Top := Top + i;
        If C is TCustomGroupBox Then
          With TCustomGroupBox(C).ChildSizing Do
            TopBottomSpacing := TopBottomSpacing + i;
{$endif darwin}
{$ifdef LCLgtk2}
        // Fix panel color bug on GTK2
        If (C is TCustomPanel) And ParentColor And (Color = clDefault) Then
          Color := clForm;
{$endif LCLgtk2}

        If C is TWinControl Then
          With TWinControl(C) Do
            For i:=0 To ControlCount - 1 Do
              DoScale(Controls[i]);
      Finally
        If C is TWinControl Then
          TWinControl(C).EnableAlign;
    End;
End;
End;

constructor TBaseForm.Create(TheOwner: TComponent);
Begin
  inherited Create(TheOwner);
  FNeedAutoSize := AutoSize;
  AutoSize := False;
End;

Procedure TBaseForm.DoCreate;
{$ifdef LCLcarbon}

Var 
  i: integer;
  {$endif LCLcarbon}
Begin
  InitScale;
  HandleNeeded;
  Font.Height := ScaleInt(-11);
  DoScale(Self);
  If FNeedAutoSize Then
    AutoSizeForm(Self);
{$ifdef LCLcarbon}
  // Destroy handles of child controls to fix the LCL Carbon bug.
  // Without this hack, it will not be possible to hide form's controls.
  For i:=0 To ControlCount - 1 Do
    If Controls[i] is TWinControl Then
      THackControl(Controls[i]).DestroyHandle;
{$endif LCLcarbon}
  inherited DoCreate;
End;

Procedure TBaseForm.InitScale;

Var 
  i: integer;
  tm: TLCLTextMetric;
Begin
  If ScaleDivider <> 0 Then exit;
  ScaleDivider := 11;
  i := Screen.SystemFont.Height;
  If i = 0 Then
    Begin
      If Canvas.GetTextMetrics(tm) Then
        Begin
          ScaleMultiplier := tm.Ascender;
          If ScaleMultiplier < 11 Then
            ScaleMultiplier := 11;
        End
      Else
        Begin
          ScaleMultiplier := Canvas.TextHeight('Wy');
          ScaleDivider := 13;
        End;
      If ScaleMultiplier = 0 Then
        ScaleMultiplier := ScaleDivider;
    End
  Else
    ScaleMultiplier := Abs(i);
  ScaleMultiplier := ScaleMultiplier*IntfScale;
  ScaleDivider := ScaleDivider*100;
End;

initialization
  {$I baseform.lrs}

End.
