object ZoomFrame: TZoomFrame
  Left = 0
  Top = 0
  Width = 443
  Height = 277
  Align = alClient
  TabOrder = 0
  OnMouseMove = FrameMouseMove
  OnMouseUp = FrameMouseUp
  OnResize = FrameResize
  object ZoomImage: TImage
    Left = 0
    Top = 0
    Width = 443
    Height = 277
    Align = alClient
    OnMouseDown = ZoomImageMouseDown
  end
end
