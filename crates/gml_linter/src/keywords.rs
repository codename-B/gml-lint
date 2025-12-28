//! GML Keywords - Lists of protected and reserved keywords from GMEdit's GmlAPI
//!
//! These keywords should not be used as identifiers.
//! 
//! Source: GMEdit v23 fnames file
//! Markers: # = constant, * = readonly, @ = instance variable

use std::collections::HashSet;

/// Basic GML keywords (from GMEdit's kwList)
pub const KEYWORDS: &[&str] = &[
    "globalvar", "var",
    "if", "then", "else", "begin", "end", "for", "while", "do", "until", "repeat",
    "switch", "case", "default", "break", "continue", "with", "exit", "return",
    "self", "other", "noone", "all", "global", "local",
    "mod", "div", "not", "and", "or", "xor", "enum",
    "function", "constructor", "new", "delete", "throw", "try", "catch", "finally",
    "static", "true", "false", "undefined",
];

/// Flow control keywords (from GMEdit's kwFlow) - these are statement delimiters
pub const FLOW_KEYWORDS: &[&str] = &[
    "if", "then", "else", "begin", "end",
    "for", "while", "do", "until", "repeat", "with", "break", "continue",
    "switch", "case", "default",
    "try", "throw", "catch", "finally",
    "exit", "return", "wait",
    "enum", "var", "globalvar", "static",
];

/// Built-in read-only constants (marked with # in GMEdit fnames)
/// These should not be assigned to
pub const READONLY_CONSTANTS: &[&str] = &[
    // Core constants
    "true", "false", "undefined", "infinity", "NaN", "pi",
    "self", "other", "all", "noone", "global",
    "pointer_invalid", "pointer_null",
    // GM build info
    "GM_build_date", "GM_version", "GM_runtime_type", "GM_runtime_version",
    "GM_project_filename", "GM_build_type", "GM_is_sandboxed",
    "_GMLINE_", "_GMFILE_", "_GMFUNCTION_",
    // Path action constants
    "path_action_stop", "path_action_restart", "path_action_continue", "path_action_reverse",
    // Timezone constants  
    "timezone_local", "timezone_utc",
    // Gamespeed constants
    "gamespeed_fps", "gamespeed_microseconds",
];

/// Built-in readonly variables (marked with * in GMEdit fnames)
/// These cannot be assigned to (via member access or direct)
pub const READONLY_VARS: &[&str] = &[
    // Instance readonly
    "id", "object_index", "path_index", "managed", "instance_count", "instance_id",
    // Time readonly
    "fps", "fps_real", "current_time", "current_year", "current_month", "current_day",
    "current_weekday", "current_hour", "current_minute", "current_second", "delta_time",
    // Room readonly
    "room_first", "room_last", "room_width", "room_height",
    // Event readonly
    "event_type", "event_number", "event_object", "event_action",
    // Display/system readonly
    "application_surface", "view_current", "display_aa", "webgl_enabled",
    "browser_width", "browser_height",
    "os_type", "os_device", "os_browser", "os_version",
    // Mouse readonly
    "mouse_x", "mouse_y",
    // Sprite readonly (calculated properties)
    "sprite_width", "sprite_height", "sprite_xoffset", "sprite_yoffset",
    "image_number", "bbox_left", "bbox_right", "bbox_top", "bbox_bottom",
    // Physics readonly
    "phy_speed", "phy_mass", "phy_inertia", "phy_com_x", "phy_com_y",
    "phy_dynamic", "phy_kinematic", "phy_sleeping",
    "phy_collision_points", "phy_collision_x", "phy_collision_y",
    "phy_col_normal_x", "phy_col_normal_y",
    "phy_position_xprevious", "phy_position_yprevious",
    // Async/data readonly
    "async_load", "event_data", "iap_data", "collision_space",
    // Exception readonly
    "message", "longMessage", "script", "stacktrace",
    // Game info readonly
    "game_id", "game_display_name", "game_project_name", "game_save_id",
    "working_directory", "temp_directory", "cache_directory", "program_directory",
    // Rollback readonly
    "player_id", "player_local", "player_avatar_url", "player_avatar_sprite",
    "player_type", "player_user_id",
];

/// Built-in instance variables (marked with @ in GMEdit fnames)
/// These are writable but reserved - users shouldn't shadow them
pub const INSTANCE_VARIABLES: &[&str] = &[
    // Position and motion
    "x", "y", "xprevious", "yprevious", "xstart", "ystart",
    "hspeed", "vspeed", "direction", "speed", "friction", "gravity", "gravity_direction",
    "in_collision_tree",
    // Path
    "path_position", "path_positionprevious", "path_speed", "path_scale",
    "path_orientation", "path_endaction",
    // Instance properties
    "solid", "persistent", "mask_index",
    // Timeline
    "timeline_index", "timeline_position", "timeline_speed", "timeline_running", "timeline_loop",
    // Visibility and rendering
    "visible", "sprite_index", "image_index", "image_speed", "depth",
    "image_xscale", "image_yscale", "image_angle", "image_alpha", "image_blend",
    "layer", "on_ui_layer", "in_sequence", "sequence_instance", "drawn_by_sequence",
    // Physics
    "phy_rotation", "phy_position_x", "phy_position_y",
    "phy_angular_velocity", "phy_linear_velocity_x", "phy_linear_velocity_y",
    "phy_speed_x", "phy_speed_y", "phy_angular_damping", "phy_linear_damping",
    "phy_bullet", "phy_fixed_rotation", "phy_active",
];

/// Reserved built-in global variables that should not be used as user variable names
/// These are neither readonly nor instance vars but still reserved
pub const RESERVED_BUILTINS: &[&str] = &[
    // Game state
    "score", "lives", "health", "room", "room_speed", "room_persistent",
    // Keyboard state
    "keyboard_key", "keyboard_lastkey", "keyboard_lastchar", "keyboard_string",
    // Argument variables
    "argument", "argument0", "argument1", "argument2", "argument3", "argument4",
    "argument5", "argument6", "argument7", "argument8", "argument9",
    "argument10", "argument11", "argument12", "argument13", "argument14", "argument15",
    "argument_count",
    // Debug mode
    "debug_mode", "font_texture_page_size",
    // Additional Feather reserved (not in GMEdit fnames but still reserved)
    "icon", "caption", "cursor",
];

/// Create a HashSet for O(1) lookup of keywords
pub fn keyword_set() -> HashSet<&'static str> {
    KEYWORDS.iter().copied().collect()
}

/// Create a HashSet for O(1) lookup of readonly constants
pub fn readonly_set() -> HashSet<&'static str> {
    READONLY_CONSTANTS.iter().copied().collect()
}

/// Check if an identifier is a reserved keyword
pub fn is_keyword(name: &str) -> bool {
    KEYWORDS.contains(&name)
}

/// Check if an identifier is a readonly constant
pub fn is_readonly(name: &str) -> bool {
    READONLY_CONSTANTS.contains(&name)
}

/// Check if an identifier is a readonly variable (can't be assigned to)
pub fn is_readonly_var(name: &str) -> bool {
    READONLY_VARS.contains(&name)
}

/// Alias for backwards compatibility
pub fn is_readonly_instance_var(name: &str) -> bool {
    is_readonly_var(name)
}

/// Check if an identifier is a reserved built-in (e.g., score, lives, room)
pub fn is_reserved_builtin(name: &str) -> bool {
    RESERVED_BUILTINS.contains(&name)
}

/// Check if an identifier is a flow control keyword
pub fn is_flow_keyword(name: &str) -> bool {
    FLOW_KEYWORDS.contains(&name)
}

/// Check if an identifier is an instance variable
pub fn is_instance_var(name: &str) -> bool {
    INSTANCE_VARIABLES.contains(&name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_detection() {
        assert!(is_keyword("if"));
        assert!(is_keyword("function"));
        assert!(is_keyword("var"));
        assert!(!is_keyword("myVariable"));
    }

    #[test]
    fn test_readonly_detection() {
        assert!(is_readonly("true"));
        assert!(is_readonly("self"));
        assert!(!is_readonly("x"));
    }
    
    #[test]
    fn test_readonly_var_detection() {
        assert!(is_readonly_var("id"));
        assert!(is_readonly_var("object_index"));
        assert!(is_readonly_var("bbox_left"));
        assert!(!is_readonly_var("x")); // x is writable
    }
    
    #[test]
    fn test_instance_var_detection() {
        assert!(is_instance_var("x"));
        assert!(is_instance_var("sprite_index"));
        assert!(!is_instance_var("score")); // score is global, not instance
    }
    
    #[test]
    fn test_reserved_builtin_detection() {
        assert!(is_reserved_builtin("score"));
        assert!(is_reserved_builtin("lives"));
        assert!(is_reserved_builtin("argument0"));
        assert!(!is_reserved_builtin("x")); // x is instance var
    }
}
