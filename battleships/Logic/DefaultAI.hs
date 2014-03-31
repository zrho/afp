module Logic.DefaultAI where

import Logic.CleverAI

-- | The AI we use by default.
-- This way, only one line of code needs to be changed
-- when switching to a different AI.
type DefaultAI = CleverAI
