unit uFileDetect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uMimeDetect;

  //==============================================================================
  // File Categories - Similar to resource categories
  //==============================================================================
type
  // uFileDetect.pas - Fix TFileCategory enum

  TFileCategory = (
    fcUnknown,      // Could not determine

    // Document/Text Files
    fcText, fcHtml, fcXml, fcJson, fcCss, fcJavaScript, fcRtf, fcMarkdown,
    fcIni, fcBatch, fcSourceCode, fcManifest, fcVersion, fcPdf,
    fcWord, fcExcel, fcPowerPoint,  // <-- Add fcPowerPoint here

    // Image Files
    fcBitmap, fcIcon, fcCursor, fcJpeg, fcPng, fcGif, fcTiff, fcSvg, fcWebP,
    fcAnimatedIcon, fcAnimatedCursor,

    // Audio Files
    fcWave, fcMp3, fcOgg, fcFlac, fcMidi, fcAac, fcWma,

    // Video Files
    fcAvi, fcMpeg, fcMp4, fcMov, fcWmv, fcFlv, fcMkv,

    // Font Files
    fcTrueType, fcOpenType, fcBitmapFont, fcVectorFont,

    // Archive Files
    fcZip, fcRar, fc7z, fcTar, fcGzip, fcBzip2, fcCab, fcIso,

    // Executable Files
    fcExe, fcDll, fcSys, fcCom, fcOcx, fcCpl, fcDriver,

    // Database Files
    fcDatabase, fcSqlite, fcAccess, fcDbf,

    // Certificate/Security
    fcCertificate, fcKey, fcCrl, fcPkcs,

    // Other
    fcBinary, fcDataFile
    );

  function FileDetect_CheckAccArchive(Stream: TStream): boolean;
  function FileDetect_CheckAccExecutable(Stream: TStream): boolean;
  function FileDetect_CheckAccPdf(Stream: TStream): boolean;
  function FileDetect_CheckAccOffice(Stream: TStream): boolean;


//==============================================================================
// Main detection function - Uses MIME + Extension + Signature
//==============================================================================
function FileDetect_FromStream(Stream: TStream): TFileCategory;
function FileDetect_FromFile(const FileName: string): TFileCategory;
function FileDetect_FromMemory(Data: Pointer; Size: integer): TFileCategory;

//==============================================================================
// Individual file checkers
//==============================================================================
function FileDetect_CheckText(Stream: TStream): boolean;
function FileDetect_CheckImage(Stream: TStream): boolean;
function FileDetect_CheckAudio(Stream: TStream): boolean;
function FileDetect_CheckVideo(Stream: TStream): boolean;
function FileDetect_CheckFont(Stream: TStream): boolean;
function FileDetect_CheckArchive(Stream: TStream): boolean;
function FileDetect_CheckExecutable(Stream: TStream): boolean;
function FileDetect_CheckPdf(Stream: TStream): boolean;

//==============================================================================
// Extension-based guessing (when no stream available)
//==============================================================================
function FileDetect_GuessFromExtension(const FileName: string): TFileCategory;

//==============================================================================
// Helper functions
//==============================================================================
function FileDetect_CategoryToString(Category: TFileCategory): string;
function FileDetect_GetMimeType(Category: TFileCategory): string;
function FileDetect_GetDescription(Category: TFileCategory): string;
function FileDetect_ValidateExtension(const FileName: string; Stream: TStream): boolean;  // Returns True if extension matches content

implementation

uses
  uDebug;

  //------------------------------------------------------------------------------
  // Signature-based detection (most accurate)
  //------------------------------------------------------------------------------

function FileDetect_CheckAccPdf(Stream: TStream): boolean;
var
  Signature: array[0..3] of byte;
  SavedPos: int64;
begin
  Result := False;
  if Stream = nil then Exit;

  SavedPos := Stream.Position;
  try
    Stream.Position := 0;
    if Stream.Read(Signature, 4) = 4 then
      Result := (Signature[0] = $25) and (Signature[1] = $50) and (Signature[2] = $44) and (Signature[3] = $46);
    Stream.Position := SavedPos;
  except
    Stream.Position := SavedPos;
  end;
end;

function FileDetect_CheckAccOffice(Stream: TStream): boolean;
var
  Signature: array[0..7] of byte;
  SavedPos: int64;
begin
  Result := False;
  if Stream = nil then Exit;

  SavedPos := Stream.Position;
  try
    Stream.Position := 0;
    if Stream.Read(Signature, 8) = 8 then
    begin
      // Old OLE compound document (DOC, XLS, PPT)
      if (Signature[0] = $D0) and (Signature[1] = $CF) and (Signature[2] = $11) and (Signature[3] = $E0) and (Signature[4] = $A1) and (Signature[5] = $B1) and (Signature[6] = $1A) and (Signature[7] = $E1) then
        Result := True
      // New Office Open XML (DOCX, XLSX, PPTX)
      else if (Signature[0] = $50) and (Signature[1] = $4B) and (Signature[2] = $03) and (Signature[3] = $04) then
        Result := True;
    end;
    Stream.Position := SavedPos;
  except
    Stream.Position := SavedPos;
  end;
end;

function FileDetect_CheckAccArchive(Stream: TStream): boolean;
var
  Signature: array[0..3] of byte;
  SavedPos: int64;
begin
  Result := False;
  if Stream = nil then Exit;

  SavedPos := Stream.Position;
  try
    Stream.Position := 0;
    if Stream.Read(Signature, 4) = 4 then
    begin
      // ZIP (PK..)
      if (Signature[0] = $50) and (Signature[1] = $4B) then
        Result := True
      // RAR (Rar!)
      else if (Signature[0] = $52) and (Signature[1] = $61) and (Signature[2] = $72) and (Signature[3] = $21) then
        Result := True
      // 7z (7z¼¯)
      else if (Signature[0] = $37) and (Signature[1] = $7A) and (Signature[2] = $BC) and (Signature[3] = $AF) then
        Result := True
      // TAR (no signature, check extension later)
      else
        Result := False;
    end;
    Stream.Position := SavedPos;
  except
    Stream.Position := SavedPos;
  end;
end;

function FileDetect_CheckAccExecutable(Stream: TStream): boolean;
var
  Signature: array[0..1] of byte;
  SavedPos: int64;
begin
  Result := False;
  if Stream = nil then Exit;

  SavedPos := Stream.Position;
  try
    Stream.Position := 0;
    if Stream.Read(Signature, 2) = 2 then
      Result := (Signature[0] = $4D) and (Signature[1] = $5A); // MZ
    Stream.Position := SavedPos;
  except
    Stream.Position := SavedPos;
  end;
end;

//------------------------------------------------------------------------------
// Main detection functions
//------------------------------------------------------------------------------

function FileDetect_FromStream(Stream: TStream): TFileCategory;
var
  MimeType: TMimeType;
  SavedPos: Int64;
  FormatInfo: TFileFormatInfo;
begin
  Result := fcUnknown;
  if Stream = nil then Exit;

  SavedPos := Stream.Position;

  try
    // 1. Use MIME detector first
    MimeType := TMimeDetector.DetectFromStream(Stream);
    FormatInfo := TMimeDetector.GetFormatInfo(MimeType);

    // 2. Convert MIME type to file category
    case MimeType of
      // Text types
      mtText:       Result := fcText;
      mtHtml:       Result := fcHtml;
      mtXml:        Result := fcXml;
      mtJson:       Result := fcJson;
      mtCss:        Result := fcCss;
      mtJavaScript: Result := fcJavaScript;
      mtRtf:        Result := fcRtf;

      // Image types
      mtBitmap:     Result := fcBitmap;
      mtIcon:       Result := fcIcon;
      mtJpeg:       Result := fcJpeg;
      mtPng:        Result := fcPng;
      mtGif:        Result := fcGif;
      mtTiff:       Result := fcTiff;

      // Audio types
      mtWave:       Result := fcWave;
      mtMp3:        Result := fcMp3;

      // Video types
      mtAvi:        Result := fcAvi;

      // Other
      mtFont:       Result := fcTrueType;
      mtArchive:    Result := fcZip;  // Generic archive
      mtExecutable: Result := fcExe;
      mtPdf:        Result := fcPdf;
      mtWord:       Result := fcWord;
      mtExcel:      Result := fcExcel;
      mtPowerPoint: Result := fcPowerPoint;  // <-- Add this
    end;

    // 3. If MIME detection failed, try specific signature checks
    if Result = fcUnknown then
    begin
      if FileDetect_CheckPdf(Stream) then
        Result := fcPdf
      else if FileDetect_CheckAccOffice(Stream) then
      begin
        // Try to distinguish between Word, Excel, PowerPoint
        // For now, default to Word
        Result := fcWord;
      end
      else if FileDetect_CheckAccArchive(Stream) then
        Result := fcZip
      else if FileDetect_CheckAccExecutable(Stream) then
        Result := fcExe;
    end;

    Stream.Position := SavedPos;

  except
    Stream.Position := SavedPos;
  end;
end;

function FileDetect_FromFile(const FileName: string): TFileCategory;
var
  Stream: TFileStream;
begin
  Result := fcUnknown;

  if not FileExists(FileName) then
    Exit;

  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := FileDetect_FromStream(Stream);

      // If detection failed, fall back to extension
      if Result = fcUnknown then
        Result := FileDetect_GuessFromExtension(FileName);

    finally
      Stream.Free;
    end;
  except
    on E: Exception do
      DebugError('FileDetect_FromFile: ' + E.Message);
  end;
end;

function FileDetect_FromMemory(Data: Pointer; Size: integer): TFileCategory;
var
  Stream: TMemoryStream;
begin
  Result := fcUnknown;

  if (Data = nil) or (Size <= 0) then
    Exit;

  Stream := TMemoryStream.Create;
  try
    Stream.WriteBuffer(Data^, Size);
    Stream.Position := 0;
    Result := FileDetect_FromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
// Extension-based guessing (fallback)
//------------------------------------------------------------------------------

function FileDetect_GuessFromExtension(const FileName: string): TFileCategory;
var
  Ext: string;
begin
  Result := fcUnknown;
  Ext := UpperCase(ExtractFileExt(FileName));

  // Text/Documents
  if (Ext = '.TXT') or (Ext = '.TEXT') then
    Result := fcText
  else if (Ext = '.HTML') or (Ext = '.HTM') then
    Result := fcHtml
  else if (Ext = '.XML') then
    Result := fcXml
  else if (Ext = '.JSON') then
    Result := fcJson
  else if (Ext = '.CSS') then
    Result := fcCss
  else if (Ext = '.JS') then
    Result := fcJavaScript
  else if (Ext = '.RTF') then
    Result := fcRtf
  else if (Ext = '.MD') then
    Result := fcMarkdown
  else if (Ext = '.INI') or (Ext = '.CFG') or (Ext = '.CONF') then
    Result := fcIni
  else if (Ext = '.BAT') or (Ext = '.CMD') then
    Result := fcBatch
  else if (Ext = '.PDF') then
    Result := fcPdf
  else if (Ext = '.DOC') or (Ext = '.DOCX') then
    Result := fcWord
  else if (Ext = '.XLS') or (Ext = '.XLSX') then
    Result := fcExcel
  else if (Ext = '.PPT') or (Ext = '.PPTX') or (Ext = '.PPS') then  // <-- Add PowerPoint
    Result := fcPowerPoint

  // Images
  else if (Ext = '.BMP') then
    Result := fcBitmap
  else if (Ext = '.ICO') or (Ext = '.CUR') then
    Result := fcIcon
  else if (Ext = '.JPG') or (Ext = '.JPEG') then
    Result := fcJpeg
  else if (Ext = '.PNG') then
    Result := fcPng
  else if (Ext = '.GIF') then
    Result := fcGif
  else if (Ext = '.TIF') or (Ext = '.TIFF') then
    Result := fcTiff
  else if (Ext = '.SVG') then
    Result := fcSvg
  else if (Ext = '.WEBP') then
    Result := fcWebP

  // Audio
  else if (Ext = '.WAV') then
    Result := fcWave
  else if (Ext = '.MP3') then
    Result := fcMp3
  else if (Ext = '.OGG') then
    Result := fcOgg
  else if (Ext = '.FLAC') then
    Result := fcFlac
  else if (Ext = '.MID') or (Ext = '.MIDI') then
    Result := fcMidi
  else if (Ext = '.AAC') then
    Result := fcAac
  else if (Ext = '.WMA') then
    Result := fcWma

  // Video
  else if (Ext = '.AVI') then
    Result := fcAvi
  else if (Ext = '.MPG') or (Ext = '.MPEG') then
    Result := fcMpeg
  else if (Ext = '.MP4') then
    Result := fcMp4
  else if (Ext = '.MOV') then
    Result := fcMov
  else if (Ext = '.WMV') then
    Result := fcWmv
  else if (Ext = '.FLV') then
    Result := fcFlv
  else if (Ext = '.MKV') then
    Result := fcMkv

  // Fonts
  else if (Ext = '.TTF') then
    Result := fcTrueType
  else if (Ext = '.OTF') then
    Result := fcOpenType
  else if (Ext = '.FON') then
    Result := fcBitmapFont

  // Archives
  else if (Ext = '.ZIP') then
    Result := fcZip
  else if (Ext = '.RAR') then
    Result := fcRar
  else if (Ext = '.7Z') then
    Result := fc7z
  else if (Ext = '.TAR') then
    Result := fcTar
  else if (Ext = '.GZ') or (Ext = '.GZIP') then
    Result := fcGzip
  else if (Ext = '.BZ2') then
    Result := fcBzip2
  else if (Ext = '.CAB') then
    Result := fcCab
  else if (Ext = '.ISO') then
    Result := fcIso

  // Executables
  else if (Ext = '.EXE') then
    Result := fcExe
  else if (Ext = '.DLL') then
    Result := fcDll
  else if (Ext = '.SYS') then
    Result := fcSys
  else if (Ext = '.COM') then
    Result := fcCom
  else if (Ext = '.OCX') then
    Result := fcOcx
  else if (Ext = '.CPL') then
    Result := fcCpl

  // Databases
  else if (Ext = '.DB') or (Ext = '.DBF') then
    Result := fcDatabase
  else if (Ext = '.SQLITE') or (Ext = '.DB3') then
    Result := fcSqlite
  else if (Ext = '.MDB') then
    Result := fcAccess

  // Certificates
  else if (Ext = '.CER') or (Ext = '.CRT') then
    Result := fcCertificate
  else if (Ext = '.KEY') then
    Result := fcKey
  else if (Ext = '.PFX') or (Ext = '.P12') then
    Result := fcPkcs
  else if (Ext = '.CRL') then
    Result := fcCrl

  // Binary
  else if (Ext = '.BIN') or (Ext = '.DAT') then
    Result := fcBinary
  else
    Result := fcUnknown;
end;

//------------------------------------------------------------------------------
// Validation function - Check if extension matches actual content
//------------------------------------------------------------------------------

function FileDetect_ValidateExtension(const FileName: string; Stream: TStream): boolean;
var
  ExtCategory, ContentCategory: TFileCategory;
begin
  Result := False;

  if (FileName = '') or (Stream = nil) then
    Exit;

  // Get category from extension
  ExtCategory := FileDetect_GuessFromExtension(FileName);

  // Get category from content
  ContentCategory := FileDetect_FromStream(Stream);

  // Compare top-level categories
  if (ExtCategory = ContentCategory) then
    Result := True
  else
  begin
    // Check if they're in the same family
    case ExtCategory of
      fcText, fcHtml, fcXml, fcJson, fcCss, fcJavaScript, fcRtf, fcMarkdown:
        Result := ContentCategory in [fcText, fcHtml, fcXml, fcJson, fcCss, fcJavaScript, fcRtf, fcMarkdown, fcIni, fcBatch];

      fcBitmap, fcJpeg, fcPng, fcGif, fcTiff, fcSvg, fcWebP:
        Result := ContentCategory in [fcBitmap, fcJpeg, fcPng, fcGif, fcTiff, fcSvg, fcWebP];

      fcWave, fcMp3, fcOgg, fcFlac, fcMidi:
        Result := ContentCategory in [fcWave, fcMp3, fcOgg, fcFlac, fcMidi];

      fcZip, fcRar, fc7z, fcTar, fcGzip, fcBzip2:
        Result := ContentCategory in [fcZip, fcRar, fc7z, fcTar, fcGzip, fcBzip2];

      fcExe, fcDll, fcSys, fcCom:
        Result := ContentCategory in [fcExe, fcDll, fcSys, fcCom];
      else
        Result := False;
    end;
  end;
end;

//------------------------------------------------------------------------------
// Individual checkers (simplified versions)
//------------------------------------------------------------------------------

function FileDetect_CheckText(Stream: TStream): boolean;
var
  Cat: TFileCategory;
begin
  Cat := FileDetect_FromStream(Stream);
  Result := Cat in [fcText, fcHtml, fcXml, fcJson, fcCss, fcJavaScript, fcRtf, fcMarkdown, fcIni, fcBatch, fcSourceCode, fcManifest, fcVersion];
end;

function FileDetect_CheckImage(Stream: TStream): boolean;
var
  Cat: TFileCategory;
begin
  Cat := FileDetect_FromStream(Stream);
  Result := Cat in [fcBitmap, fcIcon, fcCursor, fcJpeg, fcPng, fcGif, fcTiff, fcSvg, fcWebP, fcAnimatedIcon, fcAnimatedCursor];
end;

function FileDetect_CheckAudio(Stream: TStream): boolean;
var
  Cat: TFileCategory;
begin
  Cat := FileDetect_FromStream(Stream);
  Result := Cat in [fcWave, fcMp3, fcOgg, fcFlac, fcMidi, fcAac, fcWma];
end;

function FileDetect_CheckVideo(Stream: TStream): boolean;
var
  Cat: TFileCategory;
begin
  Cat := FileDetect_FromStream(Stream);
  Result := Cat in [fcAvi, fcMpeg, fcMp4, fcMov, fcWmv, fcFlv, fcMkv];
end;

function FileDetect_CheckFont(Stream: TStream): boolean;
var
  Cat: TFileCategory;
begin
  Cat := FileDetect_FromStream(Stream);
  Result := Cat in [fcTrueType, fcOpenType, fcBitmapFont, fcVectorFont];
end;

function FileDetect_CheckArchive(Stream: TStream): boolean;
var
  Cat: TFileCategory;
begin
  Cat := FileDetect_FromStream(Stream);
  Result := Cat in [fcZip, fcRar, fc7z, fcTar, fcGzip, fcBzip2, fcCab, fcIso];
end;

function FileDetect_CheckExecutable(Stream: TStream): boolean;
var
  Cat: TFileCategory;
begin
  Cat := FileDetect_FromStream(Stream);
  Result := Cat in [fcExe, fcDll, fcSys, fcCom, fcOcx, fcCpl, fcDriver];
end;

function FileDetect_CheckPdf(Stream: TStream): boolean;
begin
  Result := FileDetect_FromStream(Stream) = fcPdf;
end;

//------------------------------------------------------------------------------
// Helper functions
//------------------------------------------------------------------------------

function FileDetect_CategoryToString(Category: TFileCategory): string;
begin
  case Category of
    fcUnknown: Result := 'Unknown';

    // Text/Documents
    fcText: Result := 'Text Document';
    fcHtml: Result := 'HTML Document';
    fcXml: Result := 'XML Document';
    fcJson: Result := 'JSON Data';
    fcCss: Result := 'CSS Stylesheet';
    fcJavaScript: Result := 'JavaScript';
    fcRtf: Result := 'RTF Document';
    fcMarkdown: Result := 'Markdown';
    fcIni: Result := 'Configuration';
    fcBatch: Result := 'Batch Script';
    fcSourceCode: Result := 'Source Code';
    fcManifest: Result := 'Manifest';
    fcVersion: Result := 'Version Info';
    fcPdf: Result := 'PDF Document';
    fcWord: Result := 'Word Document';
    fcExcel: Result := 'Excel Spreadsheet';
    fcPowerPoint: Result := 'PowerPoint Presentation';  // <-- Add this

    // Images
    fcBitmap: Result := 'Bitmap Image';
    fcIcon: Result := 'Icon';
    fcCursor: Result := 'Cursor';
    fcJpeg: Result := 'JPEG Image';
    fcPng: Result := 'PNG Image';
    fcGif: Result := 'GIF Image';
    fcTiff: Result := 'TIFF Image';
    fcSvg: Result := 'SVG Image';
    fcWebP: Result := 'WebP Image';

    // Audio
    fcWave: Result := 'WAV Audio';
    fcMp3: Result := 'MP3 Audio';
    fcOgg: Result := 'OGG Audio';
    fcFlac: Result := 'FLAC Audio';
    fcMidi: Result := 'MIDI Music';
    fcAac: Result := 'AAC Audio';
    fcWma: Result := 'WMA Audio';

    // Video
    fcAvi: Result := 'AVI Video';
    fcMpeg: Result := 'MPEG Video';
    fcMp4: Result := 'MP4 Video';
    fcMov: Result := 'QuickTime Video';
    fcWmv: Result := 'WMV Video';
    fcFlv: Result := 'Flash Video';
    fcMkv: Result := 'MKV Video';

    // Fonts
    fcTrueType: Result := 'TrueType Font';
    fcOpenType: Result := 'OpenType Font';
    fcBitmapFont: Result := 'Bitmap Font';

    // Archives
    fcZip: Result := 'ZIP Archive';
    fcRar: Result := 'RAR Archive';
    fc7z: Result := '7-Zip Archive';
    fcTar: Result := 'TAR Archive';
    fcGzip: Result := 'GZIP Archive';
    fcBzip2: Result := 'BZIP2 Archive';
    fcCab: Result := 'CAB Archive';
    fcIso: Result := 'ISO Image';

    // Executables
    fcExe: Result := 'Executable';
    fcDll: Result := 'Dynamic Link Library';
    fcSys: Result := 'System File';
    fcCom: Result := 'DOS Command';
    fcOcx: Result := 'ActiveX Control';
    fcCpl: Result := 'Control Panel Applet';
    fcDriver: Result := 'Device Driver';

    // Databases
    fcDatabase: Result := 'Database File';
    fcSqlite: Result := 'SQLite Database';
    fcAccess: Result := 'Access Database';
    fcDbf: Result := 'dBase File';

    // Certificates
    fcCertificate: Result := 'Certificate';
    fcKey: Result := 'Key File';
    fcCrl: Result := 'CRL File';
    fcPkcs: Result := 'PKCS Container';

    // Other
    fcBinary: Result := 'Binary Data';
    fcDataFile: Result := 'Data File';
    else
      Result := 'Other';
  end;
end;

function FileDetect_GetMimeType(Category: TFileCategory): string;
begin
  case Category of
    fcText: Result := 'text/plain';
    fcHtml: Result := 'text/html';
    fcXml: Result := 'text/xml';
    fcJson: Result := 'application/json';
    fcCss: Result := 'text/css';
    fcJavaScript: Result := 'text/javascript';
    fcRtf: Result := 'application/rtf';
    fcPdf: Result := 'application/pdf';
    fcWord: Result := 'application/msword';
    fcExcel: Result := 'application/vnd.ms-excel';
    fcPowerPoint: Result := 'application/vnd.ms-powerpoint';  // <-- Add this
    fcBitmap: Result := 'image/bmp';
    fcJpeg: Result := 'image/jpeg';
    fcPng: Result := 'image/png';
    fcGif: Result := 'image/gif';
    fcTiff: Result := 'image/tiff';
    fcWave: Result := 'audio/wav';
    fcMp3: Result := 'audio/mpeg';
    fcAvi: Result := 'video/x-msvideo';
    fcZip: Result := 'application/zip';
    fcExe: Result := 'application/octet-stream';
    else
      Result := 'application/octet-stream';
  end;
end;

function FileDetect_GetDescription(Category: TFileCategory): string;
begin
  Result := FileDetect_CategoryToString(Category);
end;

// uFileDetect.pas - Broad Category Checkers (Kind)

//------------------------------------------------------------------------------
// Broad Category Checkers - Returns tri-state for entire file families
// -1 = Definitely NOT this kind of file
//  0 = Maybe/Uncertain - Could be this kind (testing candidates)
//  1 = Definitely IS this kind of file
//------------------------------------------------------------------------------

function FileDetect_CheckTextKind(Stream: TStream): integer;
var
  Cat: TFileCategory;
begin
  Cat := FileDetect_FromStream(Stream);

  case Cat of
    fcText, fcHtml, fcXml, fcJson, fcCss, fcJavaScript,
    fcRtf, fcMarkdown, fcIni, fcBatch, fcSourceCode,
    fcManifest, fcVersion, fcPdf, fcWord, fcExcel:
      Result := 1;

    fcUnknown:
      Result := 0;  // Could be text, uncertain

    else
      // Definitely NOT text
      if (Cat = fcBitmap) or (Cat = fcIcon) or (Cat = fcCursor) or (Cat = fcJpeg) or (Cat = fcPng) or (Cat = fcGif) or (Cat = fcWave) or (Cat = fcMp3) or (Cat = fcAvi) or (Cat = fcTrueType) or (Cat = fcZip) or (Cat = fcExe) then
        Result := -1
      else
        Result := 0;
  end;
end;

function FileDetect_CheckImageKind(Stream: TStream): integer;
var
  Cat: TFileCategory;
begin
  Cat := FileDetect_FromStream(Stream);

  case Cat of
    fcBitmap, fcIcon, fcCursor, fcJpeg, fcPng, fcGif,
    fcTiff, fcSvg, fcWebP, fcAnimatedIcon, fcAnimatedCursor:
      Result := 1;

    fcUnknown:
      Result := 0;

    else
      if (Cat = fcText) or (Cat = fcHtml) or (Cat = fcXml) or (Cat = fcWave) or (Cat = fcMp3) or (Cat = fcAvi) or (Cat = fcTrueType) or (Cat = fcZip) or (Cat = fcExe) then
        Result := -1
      else
        Result := 0;
  end;
end;

function FileDetect_CheckAudioKind(Stream: TStream): integer;
var
  Cat: TFileCategory;
begin
  Cat := FileDetect_FromStream(Stream);

  case Cat of
    fcWave, fcMp3, fcOgg, fcFlac, fcMidi, fcAac, fcWma:
      Result := 1;

    fcUnknown:
      Result := 0;

    else
      if (Cat = fcText) or (Cat = fcBitmap) or (Cat = fcIcon) or (Cat = fcAvi) or (Cat = fcTrueType) or (Cat = fcZip) or (Cat = fcExe) then
        Result := -1
      else
        Result := 0;
  end;
end;

function FileDetect_CheckVideoKind(Stream: TStream): integer;
var
  Cat: TFileCategory;
begin
  Cat := FileDetect_FromStream(Stream);

  case Cat of
    fcAvi, fcMpeg, fcMp4, fcMov, fcWmv, fcFlv, fcMkv:
      Result := 1;

    fcUnknown:
      Result := 0;

    else
      if (Cat = fcText) or (Cat = fcBitmap) or (Cat = fcIcon) or (Cat = fcWave) or (Cat = fcMp3) or (Cat = fcTrueType) or (Cat = fcZip) or (Cat = fcExe) then
        Result := -1
      else
        Result := 0;
  end;
end;

function FileDetect_CheckFontKind(Stream: TStream): integer;
var
  Cat: TFileCategory;
begin
  Cat := FileDetect_FromStream(Stream);

  case Cat of
    fcTrueType, fcOpenType, fcBitmapFont, fcVectorFont:
      Result := 1;

    fcUnknown:
      Result := 0;

    else
      if (Cat = fcText) or (Cat = fcBitmap) or (Cat = fcIcon) or (Cat = fcWave) or (Cat = fcAvi) or (Cat = fcZip) or (Cat = fcExe) then
        Result := -1
      else
        Result := 0;
  end;
end;

function FileDetect_CheckArchiveKind(Stream: TStream): integer;
var
  Cat: TFileCategory;
begin
  Cat := FileDetect_FromStream(Stream);

  case Cat of
    fcZip, fcRar, fc7z, fcTar, fcGzip, fcBzip2, fcCab, fcIso:
      Result := 1;

    fcUnknown:
      Result := 0;

    else
      if (Cat = fcText) or (Cat = fcBitmap) or (Cat = fcIcon) or (Cat = fcWave) or (Cat = fcAvi) or (Cat = fcTrueType) or (Cat = fcExe) then
        Result := -1
      else
        Result := 0;
  end;
end;

function FileDetect_CheckExecutableKind(Stream: TStream): integer;
var
  Cat: TFileCategory;
begin
  Cat := FileDetect_FromStream(Stream);

  case Cat of
    fcExe, fcDll, fcSys, fcCom, fcOcx, fcCpl, fcDriver:
      Result := 1;

    fcUnknown:
      Result := 0;

    else
      if (Cat = fcText) or (Cat = fcBitmap) or (Cat = fcIcon) or (Cat = fcWave) or (Cat = fcAvi) or (Cat = fcTrueType) or (Cat = fcZip) then
        Result := -1
      else
        Result := 0;
  end;
end;

end.
