module Limonad.Markdown.Types where

	data Markdown = Header1 String
				  | Header2 String
				  | Header3 String
				  | Header4 String
				  | Header5 String
				  | Header6 String
				  | Paragraph [Markdown] --
				  | Quote [Markdown]
				  | Emphasis String
				  | StrongEmphasis String
				  | List [Markdown]
				  | OrderedList [Markdown]
				  | Link String (Maybe String) [Markdown]
				  | ReferenceCall String String
				  | ReferenceDef String String
				  | InlineReference String String
				  | Image String (Maybe String) String
				  | ImageCall String String
				  | ImageRef String String (Maybe String)
				  | Code (Maybe String) [String]
				  | InlineClode String
				  deriving (Eq, Ord, Show)