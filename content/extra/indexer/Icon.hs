module Indexer.Icon where

import           Data.Char             (toLower)
import qualified Data.HashMap.Strict   as H
import           System.FilePath.Posix

-- | Icons

translateExt :: FilePath -> String
translateExt path =
        case H.lookup extension transMap of
            (Just ext) -> ext
            Nothing    -> ".png"
    where
        extension = map toLower $ takeExtension path

transMap :: H.HashMap String String
transMap = H.fromList [
      (".jpg", ".webp")
    , (".png", ".webp")
    , (".mp4", ".gif")
    , (".avi", ".gif")
    , (".mov", ".gif")
    ]

isImage :: FilePath -> Bool
isImage path =
        extension `elem` [".jpg", ".avi", ".mp4", ".mov", ".png"]
    where
        extension = map toLower $ takeExtension path

getIcon :: FilePath -> Bool -> Maybe String
-- ^ nothing indicates that the icon needs to be generated
getIcon fullpath exec
        | isImage fullpath = Nothing
        | otherwise        = Just $ base <> tryExtension
    where
        tryExtension =
            case H.lookup extension extMap of
                (Just icon) -> icon
                Nothing     -> tryName
        tryName =
            case H.lookup name nameMap of
                (Just icon) -> icon
                Nothing     -> tryExec
        tryExec =
            if exec
                then "exe.png"
                else defaultIcon

        name      = map toLower $ takeFileName  fullpath
        extension = map toLower $ takeExtension fullpath
        defaultIcon = "default.png"
        base = "/theme/icons/"

nameMap :: H.HashMap String String
nameMap = H.fromList [
      ("makefile",   "makefile.png")
    , ("license",    "authors.png")
    , ("dockerfile", "docker.png")
    ]

extMap :: H.HashMap String String
extMap = H.fromList [
      (".7z",   "archive.png")
    , (".bz2",  "archive.png")
    , (".cab",  "archive.png")
    , (".gz",   "archive.png")
    , (".tar",  "archive.png")
    , (".aac",  "audio.png")
    , (".aif",  "audio.png")
    , (".aifc", "audio.png")
    , (".aiff", "audio.png")
    , (".ape",  "audio.png")
    , (".au",   "audio.png")
    , (".flac", "audio.png")
    , (".iff",  "audio.png")
    , (".m4a",  "audio.png")
    , (".mid",  "audio.png")
    , (".mp3",  "audio.png")
    , (".mpa",  "audio.png")
    , (".ra",   "audio.png")
    , (".wav",  "audio.png")
    , (".wma",  "audio.png")
    , (".f4a",  "audio.png")
    , (".f4b",  "audio.png")
    , (".oga",  "audio.png")
    , (".ogg",  "audio.png")
    , (".xm",   "audio.png")
    , (".it",   "audio.png")
    , (".s3m",  "audio.png")
    , (".mod",  "audio.png")
    , (".bin",  "bin.png")
    , (".hex",  "bin.png")
    , (".bmp",  "bmp.png")
    , (".class","bin.png")
    , (".c",    "c.png")
    , (".xlsx", "calc.png")
    , (".xlsm", "calc.png")
    , (".xltx", "calc.png")
    , (".xltm", "calc.png")
    , (".xlam", "calc.png")
    , (".xlr",  "calc.png")
    , (".xls",  "calc.png")
    , (".csv",  "calc.png")
    , (".iso",  "cd.png")
    , (".cpp",  "cpp.png")
    , (".css",  "css.png")
    , (".sass", "css.png")
    , (".scss", "css.png")
    , (".deb",  "deb.png")
    , (".doc",  "doc.png")
    , (".docx", "doc.png")
    , (".docm", "doc.png")
    , (".dot",  "doc.png")
    , (".dotx", "doc.png")
    , (".dotm", "doc.png")
    , (".log",  "doc.png")
    , (".msg",  "doc.png")
    , (".odt",  "doc.png")
    , (".pages","doc.png")
    , (".rtf",  "doc.png")
    , (".tex",  "doc.png")
    , (".wpd",  "doc.png")
    , (".wps",  "doc.png")
    , (".mobi", "doc.png")
    , (".epub", "doc.png")
    , (".svg",  "draw.png")
    , (".svgz", "draw.png")
    , (".ai",   "eps.png")
    , (".eps",  "eps.png")
    , (".exe",  "exe.png")
    , (".gif",  "gif.png")
    , (".h",    "h.png")
    , (".hs",   "haskell.png")
    , (".html", "html.png")
    , (".xhtml","html.png")
    , (".shtml","html.png")
    , (".htm",  "html.png")
    , (".url",  "html.png")
    , (".ico",  "ico.png")
    , (".java", "java.png")
    , (".jar",  "java.png")
    , (".jpg",  "jpg.png")
    , (".jpeg", "jpg.png")
    , (".jpe",  "jpg.png")
    , (".js",   "js.png")
    , (".json", "js.png")
    , (".md",   "markdown.png")
    , (".cabal","makefile.png")
    , (".pkg",  "package.png")
    , (".dmg",  "package.png")
    , (".pdf",  "pdf.png")
    , (".php",  "php.png")
    , (".phtml","php.png")
    , (".m3u",  "playlist.png")
    , (".m3u8", "playlist.png")
    , (".pls",  "playlist.png")
    , (".pls8", "playlist.png")
    , (".png",  "png.png")
    , (".ps",   "ps.png")
    , (".psd",  "psd.png")
    , (".py",   "py.png")
    , (".rar",  "rar.png")
    , (".rb",   "rb.png")
    , (".rpm",  "rpm.png")
    , (".rss",  "rss.png")
    , (".bf",   "source.png")
    , (".go",   "source.png")
    , (".rkt",  "source.png")
    , (".bat",  "script.png")
    , (".cmd",  "script.png")
    , (".sh",   "script.png")
    , (".fish", "script.png")
    , (".sql",  "sql.png")
    , (".tiff", "tiff.png")
    , (".tif",  "tiff.png")
    , (".txt",  "text.png")
    , (".nfo",  "text.png")
    , (".asf",  "video.png")
    , (".asx",  "video.png")
    , (".avi",  "video.png")
    , (".flv",  "video.png")
    , (".mkv",  "video.png")
    , (".mov",  "video.png")
    , (".mp4",  "video.png")
    , (".mpg",  "video.png")
    , (".rm",   "video.png")
    , (".srt",  "video.png")
    , (".swf",  "video.png")
    , (".vob",  "video.png")
    , (".wmv",  "video.png")
    , (".m4v",  "video.png")
    , (".f4v",  "video.png")
    , (".f4p",  "video.png")
    , (".ogv",  "video.png")
    , (".xml",  "xml.png")
    , (".yml",  "rtf.png")
    , (".yaml", "rtf.png")
    , (".zip",  "zip.png")
    ]
