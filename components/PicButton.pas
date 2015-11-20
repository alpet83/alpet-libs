unit PicButton;

interface

uses
   Windows, SysUtils, Classes, Forms, Graphics, Buttons,
  StdCtrls, ExtCtrls, CommCtrl, ImgList;

type
  TPicButton = class(TSpeedButton)
  private
    { Private declarations }

    field_hack: Integer;

  protected
    { Protected declarations }
    property            Flat;

    { methods }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TPicButton]);
end;

{ TPicButton }

constructor TPicButton.Create(AOwner: TComponent);
begin
 inherited;
 Flat := TRUE;
 field_hack := $12345;
end;

procedure TPicButton.Paint;
var
   src, PaintRect: TRect;
   n_img, xofs: Integer;
   cx, cy: Integer;
   bf: BlendFunction;
   TmpImage: TBitmap;
begin
{ pb := @field_hack;
 Dec ( DWORD(pb), 7);
 pb^ := FALSE;
 if not MouseInControl then
    inherited;}

  if not Enabled then
  begin
    FState := bsDisabled;
  end
  else if FState = bsDisabled then
    if Down and (GroupIndex <> 0) then
      FState := bsExclusive
    else
      FState := bsUp;

 cx := Width;
 cy := Height;
 PaintRect := Rect(0, 0, cx, cy);

 Canvas.Lock;

 TmpImage := TBitMap.Create;
 try
  TmpImage.Width := Width;
  TmpImage.Height := Height;
  TmpImage.Canvas.Brush.Color := clBtnFace;
  TmpImage.Palette := CopyPalette(Glyph.Palette);

  //  TmpImage.Canvas.CopyRect (PaintRect, ... );

  n_img := Ord (FState);
  if n_img > self.NumGlyphs then
     n_img := 0;


  xofs := n_img * (Glyph.Width div NumGlyphs);
  src := Rect (xofs, 0, xofs + cx, cy);

  {
  TmpImage.Canvas.CopyRect (PaintRect, Canvas, PaintRect);

  TransparentBlt (TmpImage.Canvas.Handle, 0, 0, cx, cy,
                  Glyph.Canvas.Handle, src.Left, src.top, src.Right, src.Bottom, RGB (0, 0, 255) );}

  TmpImage.Canvas.CopyRect (PaintRect, Glyph.Canvas, src);

  FillChar (bf, sizeof (bf), 0);

  bf.BlendOp := AC_SRC_OVER;
  bf.AlphaFormat := AC_SRC_ALPHA;
  bf.SourceConstantAlpha := 200;



  {AlphaBlend( Canvas.Handle, 0, 0, Width, Height,
                  TmpImage.Canvas.Handle, 0, 0, Width, Height, bf );}

  TransparentBlt (Canvas.Handle, 0, 0, Width, Height,
                  TmpImage.Canvas.Handle, 0, 0, Width, Height, RGB (0, 0, 255) );

  // Canvas.CopyRect (PaintRect, TmpImage.Canvas, PaintRect);
 finally
  TmpImage.Free;
  Canvas.Unlock;
 end;
end;

end.
