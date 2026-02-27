unit uResFileIO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  uResDefs, uResManager, uMimeDetect, uDebug, uResFile;

type
  TResFileIO = class
  private
    class function GetSaveFileName(const ResourceName: string; const ResType: TResType; const MimeType: TMimeType): string;
    class function ConfirmOverwrite(const FileName: string; Param: integer): boolean;
    class procedure SaveMultipleImages(const MultiResult: TMultiResLoadResult; const BaseFileName: string; Param: integer);
    class function ExtractFirstExt(const Extensions: string): string;
  public
    class function PrepareResourceForSave(const ResourceName, ResTypeName: string; Param: integer): TMemoryStream;
    class function ExportResource(const ResourceName, ResTypeName: string; Param: integer): boolean;

    class function SaveResourceFile(const DestFileName: string): boolean;
  end;

implementation

uses
  uResUtils, FileUtil, Math;

  { TResFileIO }

  //------------------------------------------------------------------------------
  // Extraer la primera extensión de una lista (ej. ".jpg,.jpeg" -> ".jpg")
  //------------------------------------------------------------------------------
class function TResFileIO.ExtractFirstExt(const Extensions: string): string;
var
  CommaPos: integer;
begin
  CommaPos := Pos(',', Extensions);
  if CommaPos > 0 then
    Result := Copy(Extensions, 1, CommaPos - 1)
  else
    Result := Extensions;
end;

//------------------------------------------------------------------------------
// Confirmar si sobrescribir archivo existente (según parámetro)
//------------------------------------------------------------------------------
class function TResFileIO.ConfirmOverwrite(const FileName: string; Param: integer): boolean;
begin
  if not FileExists(FileName) then
    Exit(True);

  // Param=4: sobrescribir sin preguntar
  if (Param = 1) or (Param = 4) then
    Exit(True);

  // Para otros casos, preguntar
  Result := MessageDlg('File Exists', Format('File "%s" already exists. Overwrite?', [ExtractFileName(FileName)]), mtConfirmation, [mbYes, mbNo], 0) = 6;  // 6 = mrYes
end;

//------------------------------------------------------------------------------
// Obtener nombre de archivo para guardar con extensión apropiada
//------------------------------------------------------------------------------
class function TResFileIO.GetSaveFileName(const ResourceName: string; const ResType: TResType; const MimeType: TMimeType): string;
var
  Ext: string;
  FileTypes: string;
  FormatInfo: TFileFormatInfo;
begin
  FormatInfo := TMimeDetector.GetFormatInfo(MimeType);
  Ext := ExtractFirstExt(FormatInfo.Extensions);

  // Construir filtro para el diálogo correctamente: "Description (*.ext)|*.ext"
  if FormatInfo.Description <> '' then
    FileTypes := Format('%s files (*%s)|*%s|All files (*.*)|*.*', [FormatInfo.Description, Ext, Ext])
  else
    FileTypes := Format('*%s files|*%s|All files (*.*)|*.*', [Ext, Ext]);

  with TSaveDialog.Create(nil) do
  try
    Title := 'Export Resource - ' + ResourceName;
    Filter := FileTypes;
    DefaultExt := Ext;
    FileName := ResourceName + Ext;

    if Execute then
      Result := FileName
    else
      Result := '';
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------
// Guardar múltiples imágenes (caso de grupo con opción "Todas")
//------------------------------------------------------------------------------
class procedure TResFileIO.SaveMultipleImages(const MultiResult: TMultiResLoadResult; const BaseFileName: string; Param: integer);
var
  I: integer;
  FileName: string;
  Ext: string;
  SaveStream: TMemoryStream;
  FormatInfo: TFileFormatInfo;
begin
  for I := 0 to High(MultiResult.ImageStreams) do
  begin
    if MultiResult.ImageStreams[I] = nil then
      Continue;

    // Obtener extensión desde MIME type
    FormatInfo := TMimeDetector.GetFormatInfo(MultiResult.ImageMimeTypes[I]);
    Ext := ExtractFirstExt(FormatInfo.Extensions);

    FileName := Format('%s_%d%s', [BaseFileName, I, Ext]);
    FileName := ExtractFilePath(ParamStr(0)) + 'exports\' + FileName;

    ForceDirectories(ExtractFilePath(ParamStr(0)) + 'exports');

    // Confirmar sobreescritura según parámetro
    if not ConfirmOverwrite(FileName, Param) then
      Continue;

    // Crear una copia del stream para guardar
    SaveStream := TMemoryStream.Create;
    try
      MultiResult.ImageStreams[I].Position := 0;
      SaveStream.CopyFrom(MultiResult.ImageStreams[I], MultiResult.ImageStreams[I].Size);
      SaveStream.Position := 0;
      SaveStream.SaveToFile(FileName);
      DebugInfo(Format('  ✓ Exported image %d to: %s', [I, FileName]));
    except
      on E: Exception do
        DebugError(Format('  ✗ Error exporting image %d: %s', [I, E.Message]));
    end;
    SaveStream.Free;
  end;
end;

//------------------------------------------------------------------------------
// Prepara un recurso para guardar
// Devuelve:
//   - Un stream si es un recurso único o archivo combinado
//   - nil si hay múltiples imágenes y ya se procesaron
//------------------------------------------------------------------------------
class function TResFileIO.PrepareResourceForSave(const ResourceName, ResTypeName: string; Param: integer): TMemoryStream;
var
  MultiResult: TMultiResLoadResult;
  ResType: TResType;
  I: integer;
  Answer: integer;
  IsGroup: boolean;
begin
  Result := nil;

  // Interpretar el tipo de recurso
  if TryStrToInt(ResTypeName, ResType.IntValue) then
  begin
    ResType.IsInteger := True;
    IsGroup := ResType.IntValue in [12, 14];
  end
  else
  begin
    ResType.IsInteger := False;
    ResType.StrValue := ResTypeName;
    IsGroup := False;
  end;

  DebugInfo(Format('PrepareResourceForSave: "%s", Type=%s, Param=%d', [ResourceName, ResTypeName, Param]));

  // Cargar el recurso
  if ResType.IsInteger then
    MultiResult := ResMgr_LoadResource([ResourceName, IntToStr(ResType.IntValue), Ord(mrmRejoined), '', 0])
  else
    MultiResult := ResMgr_LoadResource([ResourceName, ResType.StrValue, Ord(mrmRejoined), '', 0]);

  if not MultiResult.Success or (MultiResult.Count = 0) then
  begin
    DebugError('  ✗ Failed to load resource: ' + ResourceName);
    Exit;
  end;

  // Si hay múltiples imágenes (grupo)
  if IsGroup and (Length(MultiResult.ImageStreams) > 1) then
  begin
    // Si Param = 3 o 4, guardar todas sin preguntar
    if (Param = 3) or (Param = 4) then
    begin
      DebugInfo('  Param=3/4: saving all images without dialog');
      SaveMultipleImages(MultiResult, ResourceName, Param);
      Result := nil;
      Exit;
    end;

    // Preguntar al usuario
    Answer := MessageDlg('Export Group', Format('Group contains %d images. What would you like to export?', [Length(MultiResult.ImageStreams)]), mtConfirmation, [mbYes, mbNo, mbCancel], 0);

    case Answer of
      6: // Export all images
      begin
        DebugInfo('  User chose to export all images separately');
        SaveMultipleImages(MultiResult, ResourceName, Param);
        Result := nil;
      end;

      7: // Export as single combined file
      begin
        DebugInfo('  User chose combined file');
        if MultiResult.Combined <> nil then
        begin
          Result := TMemoryStream.Create;
          MultiResult.Combined.Position := 0;
          Result.CopyFrom(MultiResult.Combined, MultiResult.Combined.Size);
          Result.Position := 0;
        end
        else if MultiResult.Count > 0 then
        begin
          Result := TMemoryStream.Create;
          MultiResult.Items[0].Data.Position := 0;
          Result.CopyFrom(MultiResult.Items[0].Data, MultiResult.Items[0].Data.Size);
          Result.Position := 0;
        end;
      end;

      2: // Cancel
      begin
        DebugInfo('  Export cancelled');
        Exit;
      end;
    end;
  end
  else if IsGroup and (Length(MultiResult.ImageStreams) = 1) then
  begin
    // Grupo con una sola imagen - devolver esa imagen
    DebugInfo('  Group with single image');
    Result := TMemoryStream.Create;
    MultiResult.Items[0].Data.Position := 0;
    Result.CopyFrom(MultiResult.Items[0].Data, MultiResult.Items[0].Data.Size);
    Result.Position := 0;
  end
  else
  begin
    // Recurso normal
    DebugInfo('  Single resource');
    Result := TMemoryStream.Create;
    MultiResult.Items[0].Data.Position := 0;
    Result.CopyFrom(MultiResult.Items[0].Data, MultiResult.Items[0].Data.Size);
    Result.Position := 0;
  end;

  // Liberar recursos (pero no el stream que devolvemos)
  for I := 0 to MultiResult.Count - 1 do
    if (MultiResult.Items[I].Data <> nil) and (MultiResult.Items[I].Data <> Result) then
      MultiResult.Items[I].Data.Free;
end;

//------------------------------------------------------------------------------
// Exportar recurso
//Parámetros 0 Show all dialogs, 1 no aski for overwrite, 2 no ask for name, 3 no ask if multiple choices, 4 no dialogs at all
//------------------------------------------------------------------------------
class function TResFileIO.ExportResource(const ResourceName, ResTypeName: string; Param: integer): boolean;
var
  PreparedStream: TMemoryStream;
  ResType: TResType;
  FileName: string;
  Ext: string;
  FormatInfo: TFileFormatInfo;
  MimeType: TMimeType;
begin
  Result := False;

  DebugInfo(Format('ExportResource: "%s", Type=%s, Param=%d', [ResourceName, ResTypeName, Param]));

  // Preparar el recurso (esto puede guardar múltiples archivos y devolver nil)
  PreparedStream := PrepareResourceForSave(ResourceName, ResTypeName, Param);

  // Si PrepareResourceForSave devuelve nil, significa que:
  // - Ya se guardaron los archivos (caso múltiples imágenes)
  // - Hubo un error o cancelación
  if PreparedStream = nil then
  begin
    // En este punto, asumimos que ya se procesó (SaveMultipleImages se encargó)
    Result := True;
    Exit;
  end;

  // Tenemos un stream único para guardar
  // Interpretar el tipo de recurso
  if TryStrToInt(ResTypeName, ResType.IntValue) then
    ResType.IsInteger := True
  else
  begin
    ResType.IsInteger := False;
    ResType.StrValue := ResTypeName;
  end;

  // Detectar MIME y obtener información del formato
  MimeType := TMimeDetector.DetectFromStream(PreparedStream);
  FormatInfo := TMimeDetector.GetFormatInfo(MimeType);
  Ext := ExtractFirstExt(FormatInfo.Extensions);

  // Para Param= 2 y 4, usar nombre fijo sin preguntar
  if (Param = 2) or (Param = 4) then
  begin
    FileName := ExtractFilePath(ParamStr(0)) + 'exports\' + ResourceName + Ext;
    ForceDirectories(ExtractFilePath(ParamStr(0)) + 'exports');
  end
  else
  begin
    FileName := GetSaveFileName(ResourceName, ResType, MimeType);
  end;

  if (FileName = '') or not ConfirmOverwrite(FileName, Param) then
  begin
    PreparedStream.Free;
    Exit;
  end;

  try
    PreparedStream.SaveToFile(FileName);
    DebugInfo(Format('  ✓ Exported to: %s (%d bytes)', [FileName, PreparedStream.Size]));
    Result := True;
  except
    on E: Exception do
      DebugError('  ✗ Error saving file: ' + E.Message);
  end;

  PreparedStream.Free;
end;

//------------------------------------------------------------------------------
// Guardar archivo .res reconstruido desde los datos en memoria Necesitamos un repaso y refactorización de esto.
//------------------------------------------------------------------------------
class function TResFileIO.SaveResourceFile(const DestFileName: string): boolean;
var
  I: integer;
  ResHeader: TResHeader;
  FileStream: TFileStream;
  DataStream: TMemoryStream;
  ResourceInfo: TResInfo;
  CurrentPos: int64;
  HeaderStart: int64;
  DataPos: int64;
  TypeStart: int64;
  NameStart: int64;
  // Variables para escritura.
  WordToWrite: word;
  DWordToWrite: DWord;
  TypeBytes: TBytes;
  NameBytes: TBytes;
  Padding: byte;
  Val: integer;
  MemoryFlagsWord: word;
  j: integer;
  PaddingBytes: integer;
  TypeStr: string;
  NameStr: string;
  TypeSize: integer;
  NameSize: integer;
  OriginalFileClosed: boolean;
  PaddingNeeded: DWord;
  NullByte: byte;
  TempFileName: string;
  MimeType: TMimeType;   // <-- AÑADIR ESTA LÍNEA
begin
  TempFileName := ChangeFileExt(DestFileName, '.tmp');
  SysUtils.DeleteFile(TempFileName);

  DebugInfo('SaveResourceFile (temp): "' + TempFileName + '"');
  DebugInfo('SaveResourceFile: "' + DestFileName + '"');

  if ResMgr_ResourceCount = 0 then
  begin
    DebugError('  No resources in memory');
    Exit;
  end;

  if ResMgr_FileStream = nil then
  begin
    DebugError('  Original file not open');
    Exit;
  end;
  OriginalFileClosed := False;
  NullByte := 0;

  Padding := 0;

  try
    //    FileStream := TFileStream.Create(DestFileName, fmCreate);
    // --- 1. Escribir el archivo temporal (el original sigue abierto) ---
    FileStream := TFileStream.Create(TempFileName, fmCreate);
    try
      // =================================================================
      // RECURSO 0: Cabecera especial de 32 bytes
      // =================================================================
      DWordToWrite := 0;
      FileStream.Write(DWordToWrite, 4);  // DataSize = 0
      DWordToWrite := 32;
      FileStream.Write(DWordToWrite, 4);  // HeaderSize = 32
      WordToWrite := $FFFF;
      FileStream.Write(WordToWrite, 2);   // $FFFF
      WordToWrite := 0;
      FileStream.Write(WordToWrite, 2);   // Tipo 0
      WordToWrite := $FFFF;
      FileStream.Write(WordToWrite, 2);   // $FFFF
      WordToWrite := 0;
      FileStream.Write(WordToWrite, 2);   // Nombre 0
      DWordToWrite := 0;
      FileStream.Write(DWordToWrite, 4);  // DataVersion
      WordToWrite := 0;
      FileStream.Write(WordToWrite, 2);   // MemoryFlags
      WordToWrite := 0;
      FileStream.Write(WordToWrite, 2);   // LanguageID
      DWordToWrite := 0;
      FileStream.Write(DWordToWrite, 4);  // Version
      DWordToWrite := 0;
      FileStream.Write(DWordToWrite, 4);  // Characteristics

      // =================================================================
      // RECURSOS NORMALES (1 .. N-1)
      // =================================================================
      for I := 1 to ResMgr_ResourceCount - 1 do
      begin
        ResourceInfo := ResMgr_Resources[I];
        HeaderStart := FileStream.Position;

        DebugInfo(Format('  Writing resource %d: Name="%s", Type=%s, DataSize=%d, DataOffset=$%x', [I, ResourceInfo.Name, ResType_ToString(ResourceInfo.ResType), ResourceInfo.DataSize, ResourceInfo.DataOffset]));

        // =================================================================
        // 1. Cargar datos del recurso (desde archivo o memoria)
        // =================================================================

        DataStream := ResMgr_LoadRawResource([0, I]);  // I es el índice actual
        if DataStream = nil then
        begin
          DebugError('  ✗ Failed to load resource data for index ' + IntToStr(I));
          Continue;
        end;
        try
          if DataStream.Size > 0 then
          begin
            DebugInfo(Format('    Loaded %d bytes for resource %d', [DataStream.Size, I]));
            // Nota: ya no usamos ResMgr_FileStream aquí
          end
          else
            DebugInfo('    Resource has no data');
          // =================================================================
          // 2. Inicializar cabecera
          // =================================================================
          FillChar(ResHeader, SizeOf(ResHeader), 0);
          ResHeader.DataSize := DataStream.Size;  // Usar tamaño real del stream
          ResHeader.DataVersion := ResourceInfo.DataVersion;
          ResHeader.MemoryFlags := ResourceInfo.MemoryFlags;
          ResHeader.LanguageID := ResourceInfo.LanguageID;
          ResHeader.Version := ResourceInfo.VersionInfo;
          ResHeader.Characteristics := ResourceInfo.Characteristics;

          // =================================================================
          // 3. Escribir DataSize y HeaderSize (temporal)
          // =================================================================
          FileStream.Write(ResHeader.DataSize, 4);
          FileStream.Write(ResHeader.HeaderSize, 4);  // Se actualizará después

          // =================================================================
          // 4. ESCRIBIR TIPO
          // =================================================================
          TypeStart := FileStream.Position;
          if ResourceInfo.ResType.IsInteger then
          begin
            // Tipo ordinal: 0xFFFF + valor
            WordToWrite := $FFFF;
            FileStream.Write(WordToWrite, 2);
            WordToWrite := ResourceInfo.ResType.IntValue;
            FileStream.Write(WordToWrite, 2);
          end
          else
          begin
            // Tipo string: UN solo null terminator
            TypeStr := ResourceInfo.ResType.StrValue + #0;
            TypeBytes := TEncoding.Unicode.GetBytes(TypeStr);
            FileStream.Write(TypeBytes[0], Length(TypeBytes));
          end;

          // =================================================================
          // 5. ESCRIBIR NOMBRE
          // =================================================================
          NameStart := FileStream.Position;
          if ResourceInfo.Name = '' then
          begin
            WordToWrite := $FFFF;
            FileStream.Write(WordToWrite, 2);
            WordToWrite := 0;
            FileStream.Write(WordToWrite, 2);
          end
          else
          begin
            Val := 0;
            if TryStrToInt(ResourceInfo.Name, Val) then
            begin
              // Nombre ordinal: 0xFFFF + valor
              WordToWrite := $FFFF;
              FileStream.Write(WordToWrite, 2);
              WordToWrite := Val and $FFFF;
              FileStream.Write(WordToWrite, 2);
            end
            else
            begin
              // Nombre string: UN solo null terminator
              NameStr := ResourceInfo.Name + #0;
              NameBytes := TEncoding.Unicode.GetBytes(NameStr);
              FileStream.Write(NameBytes[0], Length(NameBytes));
            end;
          end;
          DebugInfo(Format('Pos after name: %d, diff from start: %d', [FileStream.Position, FileStream.Position - HeaderStart]));
          PaddingNeeded := (4 - ((FileStream.Position) mod 4)) mod 4;
          DebugInfo('Padding needed: ' + IntToStr(PaddingNeeded));
          // Padding después del nombre ajustado al fichero, no a la cabecera
          if (FileStream.Position mod 4) <> 0 then
          begin
            PaddingBytes := 4 - (FileStream.Position mod 4);
            for j := 1 to PaddingBytes do
            begin
              FileStream.Write(Padding, 1);
              DebugInfo('    Added NAME Padding terminator');
            end;
          end;

          // =================================================================
          // 6. ESCRIBIR CAMPOS FIJOS (16 bytes total)
          // =================================================================
          FileStream.Write(ResHeader.DataVersion, 4);
          MemoryFlagsWord := ResHeader.MemoryFlags and $FFFF;
          FileStream.Write(MemoryFlagsWord, 2);
          FileStream.Write(ResHeader.LanguageID, 2);
          FileStream.Write(ResHeader.Version, 4);
          FileStream.Write(ResHeader.Characteristics, 4);

          // =================================================================
          // 7. ESCRIBIR DATOS
          // =================================================================
          DataPos := FileStream.Position;
          if DataStream.Size > 0 then
          begin
            DataStream.Position := 0;
            FileStream.CopyFrom(DataStream, DataStream.Size);

            // Detectar MIME para saber si es texto
            DataStream.Position := 0;
            MimeType := TMimeDetector.DetectFromStream(DataStream);

            if MimeType in [mtText, mtHtml, mtXml, mtJson, mtCss, mtJavaScript, mtRtf] then
            begin
              // Verificar si el último byte ya es null
              if DataStream.Size > 0 then
              begin
                DataStream.Position := DataStream.Size - 1;
                if DataStream.ReadByte <> 0 then
                begin
                  NullByte := 0;
                  FileStream.Write(NullByte, 1);
                  DebugInfo('    Added null terminator');

                  // Incrementar DataSize
                  ResHeader.DataSize := ResHeader.DataSize + 1;

                  // Actualizar DataSize en la cabecera (que está al inicio del recurso)
                  FileStream.Position := HeaderStart;
                  FileStream.Write(ResHeader.DataSize, 4);
                  FileStream.Seek(0, soFromEnd);  // Volver al final para continuar
                end;
              end;
            end;
          end;

          // =================================================================
          // 8. ACTUALIZAR HEADERSIZE
          // =================================================================
          ResHeader.HeaderSize := DataPos - HeaderStart;
          FileStream.Position := HeaderStart + 4;
          FileStream.Write(ResHeader.HeaderSize, 4);

          // =================================================================
          // 9. AGREGAR PADDING DWORD DESPUÉS DE LOS DATOS
          // =================================================================
          // ¡ESTO FALTABA! - Alinear a DWORD después de cada recurso
          FileStream.Seek(0, soFromEnd);
          CurrentPos := FileStream.Position;
          if (CurrentPos mod 4) <> 0 then
          begin
            PaddingBytes := 4 - (CurrentPos mod 4);
            for j := 1 to PaddingBytes do
            begin
              FileStream.Write(NullByte, 1);
              DebugInfo(Format('    Added %d bytes NullByte at offset $%x', [PaddingBytes, CurrentPos]));
            end;
          end;
        finally
          DataStream.Free;
        end;
      end;

      DebugInfo('  ✓ Successfully wrote ' + IntToStr(ResMgr_ResourceCount) + ' resources, total size: ' + IntToStr(FileStream.Size) + ' bytes');
      Result := True;
    finally
      FileStream.Free;
    end;
    // --- 2. Reemplazar el destino ---
    // Si el destino es el mismo archivo que tenemos abierto, cerrarlo primero
    if (ResMgr_FileStream <> nil) and SameText(DestFileName, ResMgr_FileName) then
    begin
      ResFile_Close(ResMgr_FileStream);
      OriginalFileClosed := True;
      DebugInfo('  Closed original file for overwrite');
    end;

    // Eliminar el destino si existe
    if FileExists(DestFileName) then
    begin
      if not SysUtils.DeleteFile(DestFileName) then
      begin
        DebugError('  ✗ Cannot delete destination file (maybe still in use)');
        SysUtils.DeleteFile(TempFileName);
        Exit;
      end;
    end;

    // Renombrar temporal a destino
    if RenameFile(TempFileName, DestFileName) then
    begin
      Result := True;
      DebugInfo('  ✓ File saved successfully: ' + DestFileName);
      // Si hemos cerrado el original, actualizamos el nombre del archivo actual (aunque esté cerrado)
      if OriginalFileClosed then
        ResMgr_FileName := DestFileName;
    end
    else
    begin
      DebugError('  ✗ Failed to rename temp file to destination');
      SysUtils.DeleteFile(TempFileName);
    end;
  except
    on E: Exception do
      DebugError('  ✗ Error saving file: ' + E.Message);
  end;

end;

end.
