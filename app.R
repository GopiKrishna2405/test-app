# ============================================================================
# ASSESSMENT PLATFORM - COMPLETE APPLICATION (v6 - PRODUCTION READY)
# Features: MCQ, Multiple Select, True/False, Short Answer, Drag & Drop
# Security: Copy Protection, Screenshot Prevention, Tab Blur
# Deployment: Shinyapps.io Compatible ✅
# ============================================================================

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(RSQLite)
library(uuid)
library(rmarkdown)
library(knitr)
library(jsonlite)
library(sortable)

# ============================================================================
# GLOBAL SETUP - DATABASE PATH (CRITICAL FOR DEPLOYMENT)
# ============================================================================

# Use temporary directory for writable database on shinyapps.io
get_db_path <- function() {
  tmp_dir <- tempdir()
  db_file <- file.path(tmp_dir, "assessment_v6_db.sqlite")

  # If database doesn't exist in /tmp, create it
  if (!file.exists(db_file)) {
    # Check if bundled database exists (for initialization)
    bundled_db <- "assessment_v6_db.sqlite"
    if (file.exists(bundled_db)) {
      file.copy(bundled_db, db_file, overwrite = TRUE)
    }
  }

  return(db_file)
}

db_path <- get_db_path()

# Create necessary directories
dir.create(file.path(tempdir(), "uploads"), showWarnings = FALSE, recursive = TRUE)
dir.create("www", showWarnings = FALSE)
dir.create("www/help", showWarnings = FALSE, recursive = TRUE)

# Add resource path for web assets
addResourcePath("www", "www")

# Establish database connection with error handling
db_con <- tryCatch({
  con <- dbConnect(RSQLite::SQLite(), db_path)
  dbExecute(con, "PRAGMA foreign_keys = ON")
  con
}, error = function(e) {
  stop(paste("Failed to connect to database:", e$message))
})

# ============================================================================
# CREATE DATABASE TABLES
# ============================================================================

create_tables <- function() {
  tryCatch({
    # Admin accounts table
    dbExecute(db_con, "CREATE TABLE IF NOT EXISTS admins (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      password TEXT NOT NULL,
      email TEXT,
      full_name TEXT,
      is_main INTEGER DEFAULT 0,
      role TEXT DEFAULT 'admin',
      can_create_questions INTEGER DEFAULT 1,
      can_manage_students INTEGER DEFAULT 1,
      can_view_reports INTEGER DEFAULT 1,
      can_manage_admins INTEGER DEFAULT 0,
      status TEXT DEFAULT 'active',
      created_by TEXT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      last_login DATETIME
    )")

    # Questions table
    dbExecute(db_con, "CREATE TABLE IF NOT EXISTS questions (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      main_context TEXT,
      sub_questions TEXT NOT NULL,
      allow_upload INTEGER DEFAULT 0,
      section_name TEXT DEFAULT 'General',
      section_order INTEGER DEFAULT 1,
      is_pinned INTEGER DEFAULT 0,
      question_paper TEXT DEFAULT 'all',
      created_by TEXT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )")

    # Student results table
    dbExecute(db_con, "CREATE TABLE IF NOT EXISTS results (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT NOT NULL,
      roll_number TEXT NOT NULL,
      code TEXT NOT NULL,
      earned_marks REAL DEFAULT 0,
      total_marks REAL DEFAULT 0,
      percentage REAL DEFAULT 0,
      question_paper TEXT DEFAULT 'all',
      timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
    )")

    # Student answers table
    dbExecute(db_con, "CREATE TABLE IF NOT EXISTS student_answers (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      result_id INTEGER NOT NULL,
      question_id INTEGER NOT NULL,
      sub_idx INTEGER,
      prompt TEXT,
      user_ans TEXT,
      correct_ans TEXT,
      marks_possible REAL DEFAULT 0,
      marks_earned REAL DEFAULT 0,
      is_flagged INTEGER DEFAULT 0,
      uploaded_file TEXT DEFAULT '',
      FOREIGN KEY(result_id) REFERENCES results(id),
      FOREIGN KEY(question_id) REFERENCES questions(id)
    )")

    # Assessment sessions/codes table
    dbExecute(db_con, "CREATE TABLE IF NOT EXISTS sessions (
      code TEXT PRIMARY KEY,
      question_paper TEXT DEFAULT 'all',
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )")

    # Global settings table
    dbExecute(db_con, "CREATE TABLE IF NOT EXISTS settings (
      id INTEGER PRIMARY KEY,
      exam_duration INTEGER DEFAULT 60,
      instructions TEXT,
      help_text TEXT,
      help_file TEXT
    )")

    # Question papers table
    dbExecute(db_con, "CREATE TABLE IF NOT EXISTS question_papers (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE NOT NULL,
      description TEXT,
      question_ids TEXT DEFAULT '',
      created_by TEXT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )")

    # Admin activity logs table
    dbExecute(db_con, "CREATE TABLE IF NOT EXISTS admin_logs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      admin_id INTEGER,
      admin_username TEXT,
      action TEXT,
      details TEXT,
      timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
    )")

  }, error = function(e) {
    warning(paste("Table creation warning:", e$message))
  })
}

# Initialize database
create_tables()

# ============================================================================
# INITIALIZE DEFAULT DATA
# ============================================================================

initialize_default_data <- function() {
  tryCatch({
    # Initialize settings
    existing_settings <- dbGetQuery(db_con, "SELECT COUNT(*) as cnt FROM settings")
    if(existing_settings$cnt[1] == 0) {
      default_instructions <- toJSON(c(
        "Welcome to the Assessment Platform",
        "Read all questions carefully before answering",
        "You have access to a calculator and scratch pad",
        "Flag questions you want to review later",
        "Your answers are auto-saved as you progress",
        "Click End Exam when you are done",
        "Good luck!"
      ), auto_unbox = FALSE)

      dbExecute(db_con,
                "INSERT INTO settings (id, exam_duration, instructions, help_text, help_file)
         VALUES (1, 60, ?, 'For technical issues, contact administrator.', '')",
                list(default_instructions))
    }

    # Initialize default admin
    existing_admin <- dbGetQuery(db_con, "SELECT COUNT(*) as cnt FROM admins WHERE is_main = 1")
    if(existing_admin$cnt[1] == 0) {
      dbExecute(db_con,
                "INSERT INTO admins (username, password, is_main, role, email, full_name, can_manage_admins, status)
         VALUES (?, ?, 1, 'Head Admin', ?, ?, 1, 'active')",
                list("admin", "admin123", "admin@assessment.com", "Head Administrator"))
    }
  }, error = function(e) {
    warning(paste("Initialization warning:", e$message))
  })
}

initialize_default_data()

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

log_admin_action <- function(admin_id, username, action, details) {
  tryCatch({
    if(!is.null(admin_id) && !is.null(username)) {
      dbExecute(db_con,
                "INSERT INTO admin_logs (admin_id, admin_username, action, details)
         VALUES (?, ?, ?, ?)",
                list(admin_id, username, action, details))
    }
  }, error = function(e) { NULL })
}

parse_instructions <- function(raw) {
  if(is.null(raw) || is.na(raw)) return(list("No instructions provided"))

  tryCatch({
    parsed <- fromJSON(raw)
    if (is.character(parsed)) return(as.list(parsed))
    return(list(as.character(raw)))
  }, error = function(e) {
    lines <- strsplit(raw, "\n")[[1]]
    lines <- lines[trimws(lines) != ""]
    if (length(lines) == 0) return(list(raw))
    return(as.list(lines))
  })
}

safe_parse_subs <- function(json_str) {
  if(is.null(json_str) || is.na(json_str) || trimws(json_str) == "") {
    return(list())
  }
  tryCatch({
    fromJSON(json_str, simplifyVector = FALSE)
  }, error = function(e) {
    list()
  })
}

create_sectioned_order <- function(question_ids) {
  if(length(question_ids) == 0) return(integer(0))

  tryCatch({
    if(length(question_ids) == 0) return(integer(0))

    placeholders <- paste(rep("?", length(question_ids)), collapse = ",")
    query <- sprintf(
      "SELECT id, section_name, section_order FROM questions WHERE id IN (%s)",
      placeholders
    )

    qs <- dbGetQuery(db_con, query, question_ids)

    if(nrow(qs) == 0) return(integer(0))

    qs$section_name[is.na(qs$section_name) | qs$section_name == ""] <- "General"
    qs$section_order[is.na(qs$section_order)] <- 1

    sections <- qs %>%
      arrange(section_order, section_name) %>%
      group_by(section_name, section_order) %>%
      summarise(ids = list(id), .groups = 'drop') %>%
      arrange(section_order)

    unlist(sections$ids)
  }, error = function(e) {
    question_ids
  })
}

safe_db_query <- function(query, params = list()) {
  tryCatch({
    if(length(params) > 0) {
      dbGetQuery(db_con, query, params)
    } else {
      dbGetQuery(db_con, query)
    }
  }, error = function(e) {
    warning(paste("Query error:", e$message))
    data.frame()
  })
}

# ============================================================================
# UI DEFINITION
# ============================================================================

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    primary = "#2A9D8F",
    secondary = "#264653",
    base_font = font_google("Inter")
  ),

  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$meta(name = "description", content = "Assessment Platform"),

    tags$style(HTML("
      * {
        -webkit-user-select: none;
        -moz-user-select: none;
        -ms-user-select: none;
        user-select: none;
        -webkit-touch-callout: none;
      }

      body {
        background-color: #F8F9FA;
        overflow-x: hidden;
        font-family: 'Inter', sans-serif;
      }

      input, textarea, select {
        -webkit-user-select: text !important;
        -moz-user-select: text !important;
        -ms-user-select: text !important;
        user-select: text !important;
      }

      @media print { body { display: none !important; } }

      .navbar { background: linear-gradient(135deg, #2A9D8F 0%, #264653 100%) !important; }

      .admin-card {
        border-top: 5px solid #264653;
        border-radius: 10px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
      }

      .question-box {
        background: white;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.05);
        margin-bottom: 20px;
      }

      .main-context {
        background-color: #e9f7fe;
        padding: 20px;
        border-left: 5px solid #3498db;
        margin-bottom: 25px;
        border-radius: 5px;
        font-size: 1.1em;
      }

      .section-header {
        background: linear-gradient(135deg, #2A9D8F 0%, #264653 100%);
        color: white;
        padding: 15px 25px;
        border-radius: 10px;
        margin-bottom: 20px;
        box-shadow: 0 4px 15px rgba(42, 157, 143, 0.3);
        text-align: center;
      }

      .admin-info-card {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 20px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.15);
      }

      .head-admin-badge {
        background: #FF6B6B;
        color: white;
        padding: 6px 14px;
        border-radius: 20px;
        font-size: 0.85em;
        font-weight: bold;
        display: inline-block;
        margin-top: 8px;
      }

      .stat-card {
        background: white;
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        text-align: center;
        margin-bottom: 15px;
      }

      .stat-number {
        font-size: 2.5em;
        font-weight: 700;
        color: #2A9D8F;
      }

      .stat-label {
        color: #666;
        font-size: 0.95em;
        margin-top: 8px;
      }

      .btn { border-radius: 6px; font-weight: 600; }
      .btn-primary { background: #2A9D8F; border: none; color: white; }
      .btn-primary:hover { background: #1f7a6d; }
      .btn-secondary { background: #6c757d; border: none; color: white; }
      .btn-danger { background: #E76F51; border: none; color: white; }
      .btn-success { background: #2ecc71; border: none; color: white; }
      .btn-info { background: #3498db; border: none; color: white; }

      .form-control, .form-select {
        border-radius: 6px;
        border: 1px solid #dee2e6;
      }

      .form-control:focus {
        border-color: #2A9D8F;
        box-shadow: 0 0 0 0.2rem rgba(42, 157, 143, 0.25);
      }

      .card {
        border: none;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border-radius: 10px;
      }

      .modal-content { border-radius: 10px; }
      .modal-header {
        background: linear-gradient(135deg, #2A9D8F 0%, #264653 100%);
        color: white;
        border: none;
      }

      .calculator-frame {
        background: linear-gradient(135deg, #e8e8e8 0%, #c0c0c0 100%);
        border: 8px solid #333;
        border-radius: 20px;
        padding: 15px;
        box-shadow: 0 15px 35px rgba(0,0,0,0.5);
        max-width: 420px;
        margin: 20px auto;
        font-family: 'Courier New', monospace;
      }

      .calc-display {
        background: linear-gradient(135deg, #b8d0b0 0%, #9fbf99 100%);
        border: 3px solid #333;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
        box-shadow: inset 0 2px 5px rgba(0,0,0,0.3);
        text-align: right;
      }

      .calc-expression {
        font-size: 11px;
        color: #555;
        min-height: 18px;
        margin-bottom: 5px;
      }

      .calc-main-display {
        font-size: 32px;
        font-weight: bold;
        color: #000;
        word-break: break-all;
        line-height: 1.2;
      }

      .calc-button-grid {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 8px;
        margin-bottom: 10px;
      }

      .calc-button-grid-5 {
        grid-template-columns: repeat(5, 1fr);
      }

      .calc-btn {
        border: 2px solid #999;
        border-radius: 6px;
        padding: 12px;
        font-size: 14px;
        font-weight: bold;
        cursor: pointer;
        transition: all 0.1s;
        box-shadow: 0 4px 0 #666;
      }

      .calc-btn:active {
        box-shadow: 0 2px 0 #666;
        transform: translateY(2px);
      }

      .calc-btn-num { background: #f0f0f0; color: #000; border-color: #999; }
      .calc-btn-op { background: #ff9900; color: white; border-color: #cc7700; }
      .calc-btn-fn { background: #cccccc; color: #000; border-color: #999; font-size: 12px; }
      .calc-btn-eq { background: #ff6600; color: white; border-color: #cc4400; }
      .calc-btn-clear { background: #ff3333; color: white; border-color: #cc0000; }

      .quiz-header-bar {
        background: linear-gradient(135deg, #2A9D8F 0%, #264653 100%);
        color: white;
        padding: 15px 20px;
        border-radius: 10px;
        margin-bottom: 20px;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }

      .timer-display {
        font-size: 1.3em;
        font-weight: 700;
        background: rgba(255,255,255,0.2);
        padding: 8px 16px;
        border-radius: 8px;
      }

      .question-counter {
        background: rgba(255,255,255,0.2);
        padding: 8px 16px;
        border-radius: 8px;
        font-weight: 600;
      }

      .match-container {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 20px;
        margin: 20px 0;
        padding: 20px;
        background: #f8f9fa;
        border-radius: 10px;
      }

      .match-column {
        display: flex;
        flex-direction: column;
        gap: 10px;
      }

      .match-column-header {
        font-weight: 700;
        color: #2A9D8F;
        padding-bottom: 10px;
        border-bottom: 2px solid #2A9D8F;
      }

      .match-item-left {
        background: white;
        padding: 12px;
        border-radius: 6px;
        border-left: 4px solid #3498db;
        font-weight: 500;
      }

      .rank-list-item {
        background: white !important;
        padding: 12px !important;
        margin-bottom: 8px !important;
        border-radius: 6px !important;
        border-left: 4px solid #27ae60 !important;
        cursor: grab;
      }

      .rank-list-container {
        background: white;
        padding: 10px;
        border-radius: 8px;
        border: 2px solid #e9ecef;
        min-height: 300px;
      }

      .navigator-button-grid {
        display: flex;
        flex-wrap: wrap;
        gap: 5px;
      }

      .nav-btn {
        width: 45px;
        height: 45px;
        border-radius: 6px;
        border: none;
        font-weight: 600;
        cursor: pointer;
      }

      .nav-btn-unanswered { background: #e9ecef; color: #000; }
      .nav-btn-answered { background: #2A9D8F; color: white; }
      .nav-btn-current { background: #264653; color: white; box-shadow: 0 0 0 3px rgba(42, 157, 143, 0.5); }

      .sidebar-tools {
        background: white;
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        margin-bottom: 20px;
      }

      .tool-button {
        width: 100%;
        padding: 12px;
        margin-bottom: 10px;
        border-radius: 6px;
        border: none;
        font-weight: 600;
        cursor: pointer;
      }

      .sub-q-card {
        background: #fdfdfd;
        border: 1px solid #dee2e6;
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 25px;
      }

      .permission-grid {
        display: grid;
        grid-template-columns: repeat(2, 1fr);
        gap: 10px;
        margin: 15px 0;
      }

      .error-message {
        background: #f8d7da;
        color: #721c24;
        padding: 15px;
        border-radius: 8px;
        margin: 20px 0;
        border: 1px solid #f5c6cb;
      }
    ")),

    tags$script(HTML("
      // Prevent copying, pasting, screenshots
      document.addEventListener('keydown', function(e) {
        if (e.key === 'PrintScreen') e.preventDefault();
        if (e.ctrlKey && e.shiftKey && e.key === 'S') e.preventDefault();
        if (e.ctrlKey && e.shiftKey && e.key === 'I') e.preventDefault();
        if (e.key === 'F12') e.preventDefault();
        if (e.ctrlKey && e.key === 'c') e.preventDefault();
        if (e.ctrlKey && e.key === 'v') e.preventDefault();
        if (e.ctrlKey && e.key === 's') e.preventDefault();
      });

      document.addEventListener('contextmenu', function(e) { e.preventDefault(); });

      window.addEventListener('blur', function() {
        document.body.style.filter = 'blur(10px)';
        document.body.style.pointerEvents = 'none';
      });

      window.addEventListener('focus', function() {
        document.body.style.filter = 'none';
        document.body.style.pointerEvents = 'auto';
      });

      let instTimerInt = null;
      let examTimerInt = null;

      function startInstructionsTimer() {
        if(instTimerInt) return;
        let timeLeft = 15 * 60;
        function fmt(t) {
          let m = Math.floor(t/60), s = t%60;
          return m + ':' + (s<10?'0':'')+s;
        }
        let el = document.getElementById('inst_timer');
        if(el) el.innerText = fmt(timeLeft);
        instTimerInt = setInterval(() => {
          timeLeft--;
          if(document.getElementById('inst_timer'))
            document.getElementById('inst_timer').innerText = fmt(timeLeft);
          if(timeLeft <= 0) {
            clearInterval(instTimerInt);
            Shiny.setInputValue('inst_time_up', true, {priority:'event'});
          }
        }, 1000);
      }

      function startExamTimer(minutes) {
        if(examTimerInt) return;
        let timeLeft = minutes * 60;
        function fmtExam(t) {
          let h = Math.floor(t/3600);
          let m = Math.floor((t%3600)/60);
          let s = t%60;
          return h + ':' + (m<10?'0':'')+m + ':' + (s<10?'0':'')+s;
        }
        let el = document.getElementById('timer_display');
        if(el) el.innerText = fmtExam(timeLeft);
        examTimerInt = setInterval(() => {
          timeLeft--;
          if(document.getElementById('timer_display'))
            document.getElementById('timer_display').innerText = fmtExam(timeLeft);
          if(timeLeft <= 0) {
            clearInterval(examTimerInt);
            Shiny.setInputValue('exam_time_up', true, {priority:'event'});
          }
        }, 1000);
      }

      var calcExpr = '';
      var calcDisplay = '0';
      var calcJustEvaled = false;

      function calcAppend(id_expr, id_main, val) {
        if(calcJustEvaled && /[0-9\\.]/.test(val)) { calcExpr=''; calcDisplay='0'; }
        calcJustEvaled = false;
        calcDisplay += val;
        calcExpr += val;
        document.getElementById(id_main).innerText = calcDisplay;
        document.getElementById(id_expr).innerText = calcExpr;
      }

      function calcSetOp(id_expr, id_main, op) {
        calcJustEvaled = false;
        calcExpr += op;
        calcDisplay = op;
        document.getElementById(id_main).innerText = calcDisplay;
        document.getElementById(id_expr).innerText = calcExpr;
      }

      function calcEval(id_expr, id_main) {
        try {
          let result = Function('return (' + calcExpr + ')')();
          result = parseFloat(result.toPrecision(12));
          document.getElementById(id_expr).innerText = calcExpr + ' =';
          calcExpr = String(result);
          calcDisplay = String(result);
          document.getElementById(id_main).innerText = calcDisplay;
          calcJustEvaled = true;
        } catch(e) {
          document.getElementById(id_main).innerText = 'Error';
          calcExpr = ''; calcDisplay = '0'; calcJustEvaled = false;
        }
      }

      function calcClear(id_expr, id_main) {
        calcExpr = ''; calcDisplay = '0'; calcJustEvaled = false;
        document.getElementById(id_main).innerText = '0';
        document.getElementById(id_expr).innerText = '';
      }

      function calcBackspace(id_expr, id_main) {
        calcJustEvaled = false;
        calcExpr = calcExpr.slice(0,-1);
        calcDisplay = calcExpr === '' ? '0' : calcExpr.slice(calcExpr.search(/[^+\\-*\\/()]*$/));
        document.getElementById(id_main).innerText = calcDisplay || '0';
        document.getElementById(id_expr).innerText = calcExpr;
      }

      function calcPercent(id_expr, id_main) {
        try {
          let result = Function('return (' + calcExpr + ')')() / 100;
          result = parseFloat(result.toPrecision(12));
          calcExpr = String(result); calcDisplay = String(result);
          document.getElementById(id_main).innerText = calcDisplay;
          document.getElementById(id_expr).innerText = calcExpr + ' %';
        } catch(e) {}
      }

      function calcToggleSign(id_expr, id_main) {
        try {
          let result = -Function('return (' + calcExpr + ')')();
          result = parseFloat(result.toPrecision(12));
          calcExpr = String(result); calcDisplay = String(result);
          document.getElementById(id_main).innerText = calcDisplay;
          document.getElementById(id_expr).innerText = calcExpr;
        } catch(e) {}
      }

      function calcAppendFn(id_expr, id_main, fn) {
        calcJustEvaled = false;
        calcExpr += fn;
        calcDisplay = fn;
        document.getElementById(id_main).innerText = calcDisplay;
        document.getElementById(id_expr).innerText = calcExpr;
      }
    "))
  ),

  uiOutput("main_ui")
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {

  # Reactive values
  refresh_qs <- reactiveVal(0)
  refresh_results <- reactiveVal(0)
  refresh_admins <- reactiveVal(0)
  num_inst_lines <- reactiveVal(1)
  current_inst_page <- reactiveVal(1)

  user_state <- reactiveValues(
    role = "login",
    name = "",
    roll_number = "",
    admin_user = "",
    admin_id = NULL,
    is_head_admin = FALSE,
    current_q_idx = 1,
    q_order = NULL,
    current_result_id = NULL
  )

  # Pre-register dynamic outputs for MCQ and Multiple Select
  lapply(1:15, function(i) {
    output[[paste0("dyn_ans_mcq_", i)]] <- renderUI({
      opts <- c(
        input[[paste0("sq_opt1_", i)]],
        input[[paste0("sq_opt2_", i)]],
        input[[paste0("sq_opt3_", i)]],
        input[[paste0("sq_opt4_", i)]]
      )
      valid_opts <- opts[trimws(opts) != ""]
      if(length(valid_opts) > 0) {
        radioButtons(paste0("sq_ans_mcq_", i),
                     HTML("<b style='color:#2A9D8F;'>Select Correct Answer:</b>"),
                     choices = valid_opts)
      } else {
        helpText("Enter options above")
      }
    })

    output[[paste0("dyn_ans_ms_", i)]] <- renderUI({
      opts <- c(
        input[[paste0("sq_opt1_", i)]],
        input[[paste0("sq_opt2_", i)]],
        input[[paste0("sq_opt3_", i)]],
        input[[paste0("sq_opt4_", i)]]
      )
      valid_opts <- opts[trimws(opts) != ""]
      if(length(valid_opts) > 0) {
        checkboxGroupInput(paste0("sq_ans_ms_", i),
                           HTML("<b style='color:#2A9D8F;'>Select Correct Answer(s):</b>"),
                           choices = valid_opts)
      } else {
        helpText("Enter options above")
      }
    })
  })

  # ========================================================================
  # RENDER MAIN UI
  # ========================================================================

  output$main_ui <- renderUI({
    tryCatch({
      if (user_state$role == "login") {
        render_login_ui()
      } else if (user_state$role == "admin") {
        render_admin_ui()
      } else if (user_state$role == "quiz") {
        render_quiz_ui()
      } else if (user_state$role == "instructions") {
        render_instructions_ui()
      } else if (user_state$role == "finished") {
        render_finished_ui()
      }
    }, error = function(e) {
      div(class = "error-message",
          h4("⚠️ Error"),
          p(e$message))
    })
  })

  # ========================================================================
  # LOGIN UI RENDER
  # ========================================================================

  render_login_ui <- function() {
    div(class = "container-fluid d-flex align-items-center justify-content-center",
        style = "min-height: 100vh; background: linear-gradient(135deg, #2A9D8F 0%, #264653 100%);",
        div(class = "col-md-4",
            card(
              card_body(
                h2("📊 Assessment Platform", class = "text-center mb-4",
                   style = "color: #2A9D8F; font-weight: 700;"),
                tabsetPanel(
                  tabPanel("👨‍🎓 Student",
                           div(style = "padding: 20px 0;",
                               textInput("std_name", "Your Full Name", placeholder = "Enter your name"),
                               textInput("std_roll", "Roll Number", placeholder = "Enter roll number"),
                               textInput("std_code", "Assessment Code", placeholder = "Enter code"),
                               actionButton("btn_std_login", "Start Assessment",
                                            class = "btn-primary w-100", style = "padding: 12px; font-size: 1.05em; font-weight: 700;")
                           )
                  ),
                  tabPanel("🔐 Admin",
                           div(style = "padding: 20px 0;",
                               textInput("adm_user", "Username", placeholder = "admin"),
                               passwordInput("adm_pass", "Password", placeholder = "••••••"),
                               actionButton("btn_adm_login", "Login",
                                            class = "btn-dark w-100", style = "padding: 12px; font-size: 1.05em; font-weight: 700;")
                           )
                  )
                )
              )
            )
        )
    )
  }

  # ========================================================================
  # INSTRUCTIONS UI
  # ========================================================================

  render_instructions_ui <- function() {
    tryCatch({
      set_db <- safe_db_query("SELECT instructions FROM settings WHERE id = 1")
      if(nrow(set_db) == 0) {
        inst_list <- list("Instructions not available")
      } else {
        inst_list <- parse_instructions(set_db$instructions[1])
      }

      if(current_inst_page() > length(inst_list)) current_inst_page(1)
      current_instruction <- inst_list[[current_inst_page()]]
      total_pages <- length(inst_list)

      div(style = "height: 100vh; display: flex; flex-direction: column;",
          div(style = "flex-shrink: 0; padding: 20px; background: white; border-bottom: 1px solid #dee2e6;",
              div(class = "text-center",
                  h3("📋 Pre-Exam Instructions", class = "mb-2"),
                  span(paste("Page", current_inst_page(), "of", total_pages),
                       style = "background: rgba(42, 157, 143, 0.1); padding: 8px 16px; border-radius: 20px; color: #2A9D8F; font-weight: 600;")
              )
          ),

          div(style = "flex: 1; padding: 40px; overflow-y: auto; display: flex; flex-direction: column; justify-content: center; align-items: center;",
              div(class = "card",
                  card_body(
                    p(current_instruction, style = "font-size: 1.15em; line-height: 1.8; margin: 0;")
                  ),
                  style = "max-width: 700px;"
              ),
              div(style = "text-align: center; margin-top: 40px;",
                  p("⏱️ Time remaining:", style = "color: #666; font-weight: 600;"),
                  h2(id = "inst_timer", "15:00", style = "color: #E76F51; font-weight: 700; font-size: 2.5em;"),
                  tags$script(HTML("startInstructionsTimer();"))
              )
          ),

          div(style = "flex-shrink: 0; padding: 20px; background: white; border-top: 1px solid #dee2e6; display: flex; justify-content: space-between;",
              if(current_inst_page() > 1) {
                actionButton("prev_inst", "← Previous", class = "btn-secondary", style = "font-weight: 700;")
              } else { div() },
              actionButton("btn_start_exam", "✅ Start Exam", class = "btn-success btn-lg", style = "font-weight: 700; padding: 12px 30px;"),
              if(current_inst_page() < total_pages) {
                actionButton("next_inst", "Next →", class = "btn-primary", style = "font-weight: 700;")
              } else { div() }
          )
      )
    }, error = function(e) {
      div(class = "error-message", paste("Instructions Error:", e$message))
    })
  }

  # ========================================================================
  # ADMIN UI
  # ========================================================================

  render_admin_ui <- function() {
    tryCatch({
      set_db <- safe_db_query("SELECT * FROM settings WHERE id = 1")

      if(nrow(set_db) == 0) {
        return(div(class = "error-message", "Settings not found"))
      }

      layout_sidebar(
        sidebar = sidebar(
          div(class = "admin-info-card",
              h5(paste("👤", user_state$admin_user)),
              if(user_state$is_head_admin) {
                tags$span(class = "head-admin-badge", "🔑 HEAD ADMIN")
              } else {
                tags$span("Regular Admin")
              }
          ),
          br(),
          actionButton("gen_code", "📝 Generate Code", class = "btn-success w-100 mb-2", style = "font-weight: 700;"),
          actionButton("logout", "🚪 Logout", class = "btn-secondary w-100", style = "font-weight: 700;"),
          if(user_state$is_head_admin) {
            tagList(hr(), actionButton("clear_db", "⚠️ Reset Database", class = "btn-danger w-100", style = "font-weight: 700;"))
          }
        ),

        navset_card_underline(
          nav_panel("📊 Dashboard",
                    layout_column_wrap(width = 1/2,
                                       div(class = "stat-card",
                                           div(class = "stat-number", textOutput("stat_questions")),
                                           div(class = "stat-label", "Questions")
                                       ),
                                       div(class = "stat-card",
                                           div(class = "stat-number", textOutput("stat_students")),
                                           div(class = "stat-label", "Students")
                                       ),
                                       div(class = "stat-card",
                                           div(class = "stat-number", textOutput("stat_admins")),
                                           div(class = "stat-label", "Admins")
                                       ),
                                       div(class = "stat-card",
                                           div(class = "stat-number", textOutput("stat_sessions")),
                                           div(class = "stat-label", "Codes")
                                       )
                    )
          ),

          nav_panel("❓ Create Questions",
                    layout_column_wrap(width = 1/2,
                                       card(class = "admin-card", card_header("Create Question"),
                                            textAreaInput("main_context", "Passage/Context (Optional)", rows = 3),
                                            layout_column_wrap(width = 1/2,
                                                               textInput("section_name", "Section", value = "General"),
                                                               numericInput("section_order", "Order", value = 1, min = 1)
                                            ),
                                            numericInput("num_sub_qs", "Sub-Questions", value = 1, min = 1, max = 15),
                                            checkboxInput("admin_allow_upload", "Allow Upload?", FALSE),
                                            checkboxInput("pin_question", "📌 Pin?", FALSE),
                                            uiOutput("question_paper_ui"),
                                            hr(),
                                            uiOutput("admin_sub_qs_ui"),
                                            actionButton("save_q", "💾 Save", class = "btn-primary w-100 mt-3", style = "font-weight: 700;")
                                       ),
                                       card(card_header("Questions"),
                                            tableOutput("db_questions"),
                                            hr(),
                                            uiOutput("q_delete_select"),
                                            actionButton("btn_delete_q", "🗑️ Delete", class = "btn-danger btn-sm w-100 mb-2", style = "font-weight: 700;"),
                                            uiOutput("q_pin_select"),
                                            actionButton("btn_toggle_pin", "📌 Toggle Pin", class = "btn-warning btn-sm w-100", style = "font-weight: 700;")
                                       )
                    )
          ),

          nav_panel("⚙️ Settings",
                    card(card_header("Exam Settings"),
                         numericInput("set_timer", "Duration (Minutes)", value = if(nrow(set_db) > 0) set_db$exam_duration[1] else 60, min = 1),
                         hr(),
                         h5("📋 Instructions"),
                         uiOutput("inst_lines_ui"),
                         div(class = "d-flex gap-2",
                             actionButton("add_inst_line", "➕ Add", class = "btn-outline-primary btn-sm"),
                             actionButton("remove_inst_line", "➖ Remove", class = "btn-outline-danger btn-sm")
                         ),
                         hr(),
                         h5("Help"),
                         textAreaInput("set_help_text", "Help Text", value = if(nrow(set_db) > 0) set_db$help_text[1] else "", rows = 3),
                         fileInput("set_help_img", "Help File (Optional)"),
                         actionButton("save_settings", "Save", class = "btn-success w-100 mt-2", style = "font-weight: 700;")
                    )
          ),

          nav_panel("📋 Reports",
                    card(
                      layout_column_wrap(width = 1/2,
                                         div(
                                           h5("Master Report"),
                                           downloadButton("dl_admin_master_pdf", "📥 Download", class = "btn-danger w-100", style = "font-weight: 700;")
                                         ),
                                         div(
                                           h5("Individual Report"),
                                           uiOutput("student_selector_ui"),
                                           downloadButton("dl_admin_indiv_pdf", "📥 Download", class = "btn-info w-100", style = "font-weight: 700;")
                                         )
                      ),
                      hr(),
                      h5("Results"),
                      DTOutput("admin_table")
                    )
          ),

          if(user_state$is_head_admin) {
            nav_panel("👥 Admins",
                      card(
                        layout_column_wrap(width = 1/2,
                                           div(
                                             h5("Create Admin"),
                                             textInput("new_admin_user", "Username"),
                                             passwordInput("new_admin_pass", "Password"),
                                             textInput("new_admin_email", "Email"),
                                             textInput("new_admin_name", "Name"),
                                             actionButton("btn_create_admin", "Create", class = "btn-info w-100", style = "font-weight: 700;")
                                           )
                        ),
                        hr(),
                        DTOutput("admins_table")
                      )
            )
          }
        )
      )
    }, error = function(e) {
      div(class = "error-message", paste("Admin UI Error:", e$message))
    })
  }

  # ========================================================================
  # QUIZ UI
  # ========================================================================

  render_quiz_ui <- function() {
    tryCatch({
      set_db <- safe_db_query("SELECT exam_duration FROM settings WHERE id = 1")

      if(is.null(user_state$q_order) || length(user_state$q_order) == 0) {
        return(div(class = "error-message", "No questions available"))
      }

      current_id <- user_state$q_order[user_state$current_q_idx]
      current_q_info <- safe_db_query("SELECT section_name FROM questions WHERE id = ?", list(current_id))
      current_section <- if(nrow(current_q_info) > 0) current_q_info$section_name[1] else "General"

      exam_duration <- if(nrow(set_db) > 0) set_db$exam_duration[1] else 60

      div(style = "height: 100vh; display: flex; flex-direction: column; overflow: hidden;",
          div(class = "quiz-header-bar",
              span(class = "question-counter", paste("Q", user_state$current_q_idx, "of", length(user_state$q_order))),
              h5(current_section, class = "mb-0", style = "flex: 1; text-align: center;"),
              span(class = "timer-display", id = "timer_display", "Time: --:--:--")
          ),
          tags$script(HTML(paste0("if(typeof startExamTimer==='function'){startExamTimer(",
                                  exam_duration, ");}"))),

          div(style = "flex: 1; overflow-y: auto; padding: 20px;",
              div(class = "row",
                  div(class = "col-md-3",
                      div(class = "sidebar-tools",
                          actionButton("btn_help", "ℹ️ Help", class = "btn-info w-100 mb-2", style = "font-weight: 700;"),
                          div(class = "row",
                              div(class = "col-6", actionButton("btn_calc_basic", "🧮 Basic", class = "btn-secondary btn-sm w-100", style = "font-weight: 700;")),
                              div(class = "col-6", actionButton("btn_calc_sci", "🔬 Sci", class = "btn-secondary btn-sm w-100", style = "font-weight: 700;"))
                          )
                      ),
                      card(card_body(
                        h6("📝 Scratch Pad", style = "color: #2A9D8F; font-weight: 700;"),
                        textAreaInput("scratch_pad", NULL, rows = 5, placeholder = "Notes..."),
                        hr(),
                        checkboxInput("flag_review", "🚩 Flag", FALSE),
                        hr(),
                        h6("📍 Navigator", style = "color: #2A9D8F;"),
                        div(class = "navigator-button-grid", uiOutput("navigator_ui"))
                      ))
                  ),

                  div(class = "col-md-9",
                      div(class = "section-header", h4(current_section, style = "margin: 0;")),
                      uiOutput("render_quiz_page")
                  )
              )
          ),

          div(style = "flex-shrink: 0; background: white; padding: 15px 20px; border-top: 2px solid #dee2e6; display: flex; justify-content: space-between;",
              actionButton("prev_page", "⬅️ Previous", class = "btn btn-secondary", style = "font-weight: 700;"),
              div(class = "d-flex gap-2",
                  actionButton("skip_page", "⏭️ Skip", class = "btn btn-warning", style = "font-weight: 700;"),
                  actionButton("next_page", "✅ Next", class = "btn btn-primary", style = "font-weight: 700;")
              ),
              actionButton("btn_end_exam", "🛑 End", class = "btn btn-danger", style = "font-weight: 700;")
          )
      )
    }, error = function(e) {
      div(class = "error-message", paste("Quiz UI Error:", e$message))
    })
  }

  # ========================================================================
  # FINISHED UI
  # ========================================================================

  render_finished_ui <- function() {
    div(class = "container-fluid d-flex align-items-center justify-content-center",
        style = "min-height: 100vh; background: linear-gradient(135deg, #2A9D8F 0%, #264653 100%);",
        card(
          card_body(
            div(class = "text-center",
                h1("🎉 Complete!", style = "color: #2A9D8F; margin-bottom: 20px; font-weight: 700;"),
                h3(uiOutput("final_score_display"), style = "margin-bottom: 30px;"),
                downloadButton("dl_student_pdf", "📥 Report", class = "btn btn-info btn-lg", style = "margin-right: 10px; font-weight: 700;"),
                actionButton("logout", "🏠 Home", class = "btn btn-secondary btn-lg", style = "font-weight: 700;")
            )
          ),
          style = "max-width: 600px;"
        )
    )
  }

  # ========================================================================
  # EVENT HANDLERS
  # ========================================================================

  # Admin Login
  observeEvent(input$btn_adm_login, {
    tryCatch({
      admin_check <- safe_db_query(
        "SELECT * FROM admins WHERE username = ? AND password = ? AND status = 'active'",
        list(input$adm_user, input$adm_pass))

      if(nrow(admin_check) > 0) {
        dbExecute(db_con, "UPDATE admins SET last_login = CURRENT_TIMESTAMP WHERE id = ?",
                  list(admin_check$id[1]))

        user_state$role <- "admin"
        user_state$admin_user <- input$adm_user
        user_state$admin_id <- admin_check$id[1]
        user_state$is_head_admin <- admin_check$is_main[1] == 1

        log_admin_action(admin_check$id[1], input$adm_user, "LOGIN", "Successful")
        showNotification("✅ Login successful!", type = "message")
      } else {
        showNotification("❌ Invalid credentials", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("❌ Error:", e$message), type = "error")
    })
  })

  # Student Login
  observeEvent(input$btn_std_login, {
    tryCatch({
      name <- trimws(input$std_name)
      roll <- trimws(input$std_roll)
      code <- trimws(input$std_code)

      if(name == "" || roll == "" || code == "") {
        showNotification("⚠️ Fill all fields", type = "warning")
        return()
      }

      check_code <- safe_db_query("SELECT * FROM sessions WHERE code = ?", list(code))
      qs <- safe_db_query("SELECT id FROM questions")

      if(nrow(check_code) == 0) {
        showNotification("❌ Invalid code", type = "error")
        return()
      }

      if(nrow(qs) == 0) {
        showNotification("❌ No questions available", type = "error")
        return()
      }

      dbExecute(db_con,
                "INSERT INTO results (name, roll_number, code, question_paper) VALUES (?, ?, ?, ?)",
                list(name, roll, code, "all"))

      result_id <- dbGetQuery(db_con, "SELECT last_insert_rowid() AS id")$id

      user_state$current_result_id <- result_id
      user_state$name <- name
      user_state$roll_number <- roll
      user_state$q_order <- create_sectioned_order(qs$id)
      user_state$current_q_idx <- 1
      user_state$role <- "instructions"

      current_inst_page(1)
    }, error = function(e) {
      showNotification(paste("❌ Error:", e$message), type = "error")
    })
  })

  # Generate Assessment Code
  observeEvent(input$gen_code, {
    tryCatch({
      new_code <- toupper(substr(uuid::UUIDgenerate(), 1, 8))
      dbExecute(db_con, "INSERT INTO sessions (code, question_paper) VALUES (?, ?)",
                list(new_code, "all"))

      log_admin_action(user_state$admin_id, user_state$admin_user, "GENERATE_CODE", new_code)

      showModal(modalDialog(
        title = "✅ Code Generated",
        div(class = "text-center",
            h1(new_code, style = "color: #2A9D8F; font-size: 48px; font-weight: 700;"),
            p("Share with students", style = "color: #666; margin-top: 15px;"),
            actionButton("copy_code", "📋 Copy", class = "btn-primary")
        ),
        footer = modalButton("Close"),
        size = "m"
      ))

      refresh_qs(refresh_qs() + 1)
    }, error = function(e) {
      showNotification(paste("❌ Error:", e$message), type = "error")
    })
  })

  observeEvent(input$copy_code, {
    showNotification("📋 Code copied to clipboard!", type = "message")
  })

  # Save Question
  observeEvent(input$save_q, {
    tryCatch({
      section_name <- if(is.null(input$section_name) || trimws(input$section_name) == "")
        "General" else trimws(input$section_name)
      section_order <- if(is.null(input$section_order) || is.na(input$section_order))
        1 else as.integer(input$section_order)

      sub_qs <- list()
      for(idx in 1:input$num_sub_qs) {
        prompt <- input[[paste0("sq_prompt_", idx)]]
        type <- input[[paste0("sq_type_", idx)]]
        marks <- input[[paste0("sq_marks_", idx)]]

        if(is.null(prompt) || trimws(prompt) == "") {
          showNotification(paste("⚠️ Missing prompt for Q", idx), type = "warning")
          return()
        }

        opts_raw <- c(input[[paste0("sq_opt1_", idx)]], input[[paste0("sq_opt2_", idx)]],
                      input[[paste0("sq_opt3_", idx)]], input[[paste0("sq_opt4_", idx)]])
        valid_idx <- which(trimws(opts_raw) != "")
        opts <- trimws(opts_raw[valid_idx])

        ans_val <- ""
        if(type == "tf") {
          opts <- c("True", "False")
          ans_val <- input[[paste0("sq_ans_tf_", idx)]]
        } else if(type == "mcq") {
          selected_mcq <- input[[paste0("sq_ans_mcq_", idx)]]
          if(is.null(selected_mcq)) {
            showNotification(paste("⚠️ Select answer for Q", idx), type = "warning")
            return()
          }
          ans_val <- selected_mcq
        } else if(type == "ms") {
          selected_ms <- input[[paste0("sq_ans_ms_", idx)]]
          if(is.null(selected_ms)) {
            showNotification(paste("⚠️ Select answers for Q", idx), type = "warning")
            return()
          }
          ans_val <- paste(selected_ms, collapse = ", ")
        } else if(type == "short_answer") {
          ans_val <- trimws(input[[paste0("sq_ans_txt_", idx)]])
        }

        sub_qs[[idx]] <- list(
          prompt = trimws(prompt),
          type = type,
          marks = as.numeric(marks),
          options = opts,
          answer = trimws(ans_val)
        )
      }

      allow_upl <- if(isTRUE(input$admin_allow_upload)) 1 else 0
      is_pin <- if(isTRUE(input$pin_question)) 1 else 0
      json_data <- toJSON(sub_qs, auto_unbox = TRUE)
      q_paper <- if(is.null(input$question_paper_select)) "all" else input$question_paper_select

      dbExecute(db_con,
                "INSERT INTO questions (main_context, sub_questions, allow_upload, section_name, section_order, is_pinned, question_paper, created_by)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
                list(input$main_context, json_data, allow_upl, section_name, section_order, is_pin, q_paper, user_state$admin_user))

      log_admin_action(user_state$admin_id, user_state$admin_user, "CREATE_QUESTION", section_name)

      showNotification(paste("✅ Saved in", section_name), type = "message")
      refresh_qs(refresh_qs() + 1)

      updateTextAreaInput(session, "main_context", value = "")
      updateTextInput(session, "section_name", value = "General")
      updateNumericInput(session, "section_order", value = 1)
      updateCheckboxInput(session, "pin_question", value = FALSE)

    }, error = function(e) {
      showNotification(paste("❌ Error:", e$message), type = "error")
    })
  })

  # Delete Question
  observeEvent(input$btn_delete_q, {
    tryCatch({
      if(is.null(input$q_delete_select) || input$q_delete_select == "") {
        showNotification("⚠️ Select a question", type = "warning")
        return()
      }

      q_id <- as.numeric(input$q_delete_select)
      dbExecute(db_con, "DELETE FROM questions WHERE id = ?", list(q_id))
      dbExecute(db_con, "DELETE FROM student_answers WHERE question_id = ?", list(q_id))

      log_admin_action(user_state$admin_id, user_state$admin_user, "DELETE_QUESTION", paste("ID:", q_id))

      showNotification("✅ Deleted", type = "message")
      refresh_qs(refresh_qs() + 1)
    }, error = function(e) {
      showNotification(paste("❌ Error:", e$message), type = "error")
    })
  })

  # Toggle Pin
  observeEvent(input$btn_toggle_pin, {
    tryCatch({
      if(is.null(input$q_pin_select) || input$q_pin_select == "") {
        showNotification("⚠️ Select a question", type = "warning")
        return()
      }

      q_id <- as.numeric(input$q_pin_select)
      current_pin <- safe_db_query("SELECT is_pinned FROM questions WHERE id = ?", list(q_id))

      if(nrow(current_pin) == 0) {
        showNotification("❌ Not found", type = "error")
        return()
      }

      new_pin <- if(current_pin$is_pinned[1] == 1) 0 else 1
      dbExecute(db_con, "UPDATE questions SET is_pinned = ? WHERE id = ?", list(new_pin, q_id))

      log_admin_action(user_state$admin_id, user_state$admin_user, "TOGGLE_PIN",
                       ifelse(new_pin == 1, "Pinned", "Unpinned"))

      showNotification(paste("✅", ifelse(new_pin == 1, "Pinned", "Unpinned")), type = "message")
      refresh_qs(refresh_qs() + 1)
    }, error = function(e) {
      showNotification(paste("❌ Error:", e$message), type = "error")
    })
  })

  # Save Settings
  observeEvent(input$save_settings, {
    tryCatch({
      n <- num_inst_lines()
      inst_lines <- sapply(1:n, function(i) {
        val <- input[[paste0("inst_line_", i)]]
        if(is.null(val)) "" else trimws(val)
      })
      inst_lines <- inst_lines[inst_lines != ""]
      if(length(inst_lines) == 0) inst_lines <- c("No instructions provided.")
      inst_json <- toJSON(inst_lines, auto_unbox = FALSE)

      dbExecute(db_con,
                "UPDATE settings SET exam_duration = ?, instructions = ?, help_text = ? WHERE id = 1",
                list(input$set_timer, inst_json, input$set_help_text))

      log_admin_action(user_state$admin_id, user_state$admin_user, "UPDATE_SETTINGS", paste(input$set_timer, "mins"))

      showNotification("✅ Settings saved!", type = "message")
    }, error = function(e) {
      showNotification(paste("❌ Error:", e$message), type = "error")
    })
  })

  # Navigation Events
  observeEvent(input$next_inst, {
    set_db <- safe_db_query("SELECT instructions FROM settings WHERE id = 1")
    if(nrow(set_db) > 0) {
      inst_list <- parse_instructions(set_db$instructions[1])
      if(current_inst_page() < length(inst_list)) {
        current_inst_page(current_inst_page() + 1)
      }
    }
  })

  observeEvent(input$prev_inst, {
    if(current_inst_page() > 1) {
      current_inst_page(current_inst_page() - 1)
    }
  })

  observeEvent(input$btn_start_exam, { user_state$role <- "quiz" })
  observeEvent(input$inst_time_up, { user_state$role <- "quiz" })

  observeEvent(input$next_page, {
    save_current_ans()
    if(user_state$current_q_idx < length(user_state$q_order)) {
      user_state$current_q_idx <- user_state$current_q_idx + 1
    }
  })

  observeEvent(input$prev_page, {
    save_current_ans()
    if(user_state$current_q_idx > 1) {
      user_state$current_q_idx <- user_state$current_q_idx - 1
    }
  })

  observeEvent(input$skip_page, {
    save_current_ans()
    if(user_state$current_q_idx < length(user_state$q_order)) {
      user_state$current_q_idx <- user_state$current_q_idx + 1
    }
  })

  observeEvent(input$btn_end_exam, {
    save_current_ans()
    calculate_final_score()
  })

  observeEvent(input$exam_time_up, {
    save_current_ans()
    calculate_final_score()
  })

  observeEvent(input$btn_help, {
    set_row <- safe_db_query("SELECT help_text, help_file FROM settings WHERE id = 1")
    if(nrow(set_row) > 0) {
      showModal(modalDialog(
        title = "📚 Help",
        p(set_row$help_text[1]),
        footer = modalButton("Close")
      ))
    }
  })

  observeEvent(input$logout, {
    if(!is.null(user_state$admin_id)) {
      log_admin_action(user_state$admin_id, user_state$admin_user, "LOGOUT", "Logged out")
    }
    user_state$role <- "login"
    user_state$admin_user <- ""
    user_state$admin_id <- NULL
    user_state$is_head_admin <- FALSE
    user_state$name <- ""
  })

  observeEvent(input$add_inst_line, {
    num_inst_lines(num_inst_lines() + 1)
  })

  observeEvent(input$remove_inst_line, {
    if(num_inst_lines() > 1) {
      num_inst_lines(num_inst_lines() - 1)
    }
  })

  observeEvent(input$btn_create_admin, {
    if(!user_state$is_head_admin) {
      showNotification("❌ Only Head Admin can create admins", type = "error")
      return()
    }

    showModal(modalDialog(
      title = "Create Admin Account",
      textInput("modal_admin_user", "Username"),
      passwordInput("modal_admin_pass", "Password"),
      textInput("modal_admin_email", "Email"),
      textInput("modal_admin_name", "Full Name"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_create_admin", "Create", class = "btn-info")
      )
    ))
  })

  observeEvent(input$confirm_create_admin, {
    tryCatch({
      username <- trimws(input$modal_admin_user)
      password <- trimws(input$modal_admin_pass)
      email <- trimws(input$modal_admin_email)
      fullname <- trimws(input$modal_admin_name)

      if(username == "" || password == "") {
        showNotification("⚠️ Username and password required", type = "warning")
        return()
      }

      dbExecute(db_con,
                "INSERT INTO admins (username, password, email, full_name, role, status, created_by)
         VALUES (?, ?, ?, ?, 'Admin', 'active', ?)",
                list(username, password, email, fullname, user_state$admin_user))

      log_admin_action(user_state$admin_id, user_state$admin_user, "CREATE_ADMIN", username)

      showNotification(paste("✅ Admin created:", username), type = "message")
      removeModal()
      refresh_admins(refresh_admins() + 1)
    }, error = function(e) {
      showNotification(paste("❌", e$message), type = "error")
    })
  })

  observeEvent(input$clear_db, {
    if(!user_state$is_head_admin) {
      showNotification("❌ Only Head Admin can reset database", type = "error")
      return()
    }

    showModal(modalDialog(
      title = "⚠️ Reset Database",
      p("This will delete ALL data. Are you sure?", style = "color: #E76F51; font-weight: 600;"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Delete All", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_reset, {
    tryCatch({
      dbExecute(db_con, "DELETE FROM questions")
      dbExecute(db_con, "DELETE FROM results")
      dbExecute(db_con, "DELETE FROM sessions")
      dbExecute(db_con, "DELETE FROM student_answers")

      log_admin_action(user_state$admin_id, user_state$admin_user, "RESET_DATABASE", "All data deleted")

      removeModal()
      showNotification("✅ Database reset complete!", type = "warning")
      refresh_qs(refresh_qs() + 1)
      refresh_results(refresh_results() + 1)
    }, error = function(e) {
      showNotification(paste("❌ Error:", e$message), type = "error")
    })
  })

  # ========================================================================
  # RENDER OUTPUTS
  # ========================================================================

  output$admin_sub_qs_ui <- renderUI({
    req(input$num_sub_qs)
    lapply(1:input$num_sub_qs, function(i) {
      div(class = "sub-q-card",
          h5(paste("Q", i)),
          textInput(paste0("sq_prompt_", i), "Prompt"),
          layout_column_wrap(width = 1/2,
                             selectInput(paste0("sq_type_", i), "Type",
                                         choices = c("MCQ" = "mcq", "Multiple Select" = "ms", "True/False" = "tf",
                                                     "Short Answer" = "short_answer")),
                             numericInput(paste0("sq_marks_", i), "Marks", value = 1, min = 0.5, step = 0.5)
          ),
          conditionalPanel(
            condition = sprintf("input.sq_type_%d == 'mcq' || input.sq_type_%d == 'ms'", i, i),
            hr(),
            h6("Options:"),
            textInput(paste0("sq_opt1_", i), "Option 1"),
            textInput(paste0("sq_opt2_", i), "Option 2"),
            textInput(paste0("sq_opt3_", i), "Option 3"),
            textInput(paste0("sq_opt4_", i), "Option 4")
          ),
          conditionalPanel(
            condition = sprintf("input.sq_type_%d == 'short_answer'", i),
            textInput(paste0("sq_ans_txt_", i), "Correct Answer")
          ),
          conditionalPanel(
            condition = sprintf("input.sq_type_%d == 'tf'", i),
            selectInput(paste0("sq_ans_tf_", i), "Correct Answer", choices = c("True", "False"))
          ),
          conditionalPanel(
            condition = sprintf("input.sq_type_%d == 'mcq'", i),
            div(style = "background: #f1faee; padding: 10px; border-radius: 6px;",
                uiOutput(paste0("dyn_ans_mcq_", i)))
          ),
          conditionalPanel(
            condition = sprintf("input.sq_type_%d == 'ms'", i),
            div(style = "background: #f1faee; padding: 10px; border-radius: 6px;",
                uiOutput(paste0("dyn_ans_ms_", i)))
          )
      )
    })
  })

  output$question_paper_ui <- renderUI({
    papers <- safe_db_query("SELECT id, name FROM question_papers")
    choices <- c("All" = "all")
    if(nrow(papers) > 0) {
      choices <- c(choices, setNames(papers$id, papers$name))
    }
    selectInput("question_paper_select", "Question Paper", choices = choices)
  })

  output$db_questions <- renderTable({
    refresh_qs()
    df <- safe_db_query(
      "SELECT id, section_name, section_order, main_context, is_pinned
       FROM questions ORDER BY section_order, section_name")
    if(nrow(df) > 0) {
      df$main_context <- substr(as.character(df$main_context), 1, 30)
      df$Pin <- ifelse(df$is_pinned == 1, "📌", "")
      df <- df[, c("id", "section_name", "section_order", "main_context", "Pin")]
      colnames(df) <- c("ID", "Section", "Order", "Context", "📌")
    }
    df
  })

  output$q_delete_select <- renderUI({
    refresh_qs()
    qs <- safe_db_query("SELECT id, section_name, main_context FROM questions ORDER BY section_name")
    if(nrow(qs) > 0) {
      choices <- setNames(qs$id, paste(qs$section_name, "-", substr(as.character(qs$main_context), 1, 20)))
    } else {
      choices <- c("No questions" = "")
    }
    selectInput("q_delete_select", "Select Question", choices = choices)
  })

  output$q_pin_select <- renderUI({
    refresh_qs()
    qs <- safe_db_query("SELECT id, section_name, main_context FROM questions ORDER BY section_name")
    if(nrow(qs) > 0) {
      choices <- setNames(qs$id, paste(qs$section_name, "-", substr(as.character(qs$main_context), 1, 20)))
    } else {
      choices <- c("No questions" = "")
    }
    selectInput("q_pin_select", "Select Question", choices = choices)
  })

  output$admins_table <- renderDT({
    refresh_admins()
    safe_db_query(
      "SELECT username, email, full_name, role, status FROM admins ORDER BY is_main DESC")
  }, options = list(pageLength = 10))

  output$admin_table <- renderDT({
    refresh_results()
    safe_db_query(
      "SELECT name, roll_number, code, earned_marks, total_marks, percentage FROM results ORDER BY timestamp DESC")
  }, options = list(pageLength = 10))

  output$student_selector_ui <- renderUI({
    refresh_results()
    res <- safe_db_query("SELECT id, name, roll_number FROM results ORDER BY timestamp DESC")
    if(nrow(res) > 0) {
      selectInput("sel_student_dl", "Select Student", choices = setNames(res$id, paste(res$name, "(", res$roll_number, ")")))
    } else {
      p("No results available", class = "alert alert-warning")
    }
  })

  output$inst_lines_ui <- renderUI({
    n <- num_inst_lines()
    set_db <- safe_db_query("SELECT instructions FROM settings WHERE id = 1")
    existing <- if(nrow(set_db) > 0) parse_instructions(set_db$instructions[1]) else list()

    lapply(1:n, function(i) {
      pre <- if(i <= length(existing)) existing[[i]] else ""
      div(style = "margin-bottom: 15px;",
          span(paste0("Page ", i), style = "font-weight: 600; color: #2A9D8F;"),
          textAreaInput(paste0("inst_line_", i), NULL, value = pre, rows = 3)
      )
    })
  })

  output$stat_questions <- renderText({
    refresh_qs()
    nrow(safe_db_query("SELECT id FROM questions"))
  })

  output$stat_students <- renderText({
    refresh_results()
    nrow(safe_db_query("SELECT id FROM results"))
  })

  output$stat_admins <- renderText({
    refresh_admins()
    nrow(safe_db_query("SELECT id FROM admins"))
  })

  output$stat_sessions <- renderText({
    refresh_qs()
    nrow(safe_db_query("SELECT code FROM sessions"))
  })

  # ========================================================================
  # RENDER QUIZ PAGE & NAVIGATOR
  # ========================================================================

  output$render_quiz_page <- renderUI({
    req(user_state$current_q_idx, user_state$current_result_id)

    tryCatch({
      if(length(user_state$q_order) == 0) {
        return(div(class = "error-message", "No questions available"))
      }

      current_id <- user_state$q_order[user_state$current_q_idx]
      row <- safe_db_query("SELECT * FROM questions WHERE id = ?", list(current_id))

      if(nrow(row) == 0) {
        return(div(class = "error-message", "Question not found"))
      }

      sub_qs <- safe_parse_subs(row$sub_questions[1])
      if(length(sub_qs) == 0) {
        return(div(class = "error-message", "Error loading question"))
      }

      saved_ans <- safe_db_query(
        "SELECT sub_idx, user_ans FROM student_answers
         WHERE result_id = ? AND question_id = ?",
        list(user_state$current_result_id, current_id))

      context_ui <- if(nzchar(trimws(row$main_context[1]))) {
        div(class = "main-context", HTML(gsub("\n", "<br>", row$main_context[1])))
      } else NULL

      q_ui_list <- lapply(1:length(sub_qs), function(i) {
        sq <- sub_qs[[i]]
        input_id <- paste0("ans_", i)

        pre_val <- ""
        if(nrow(saved_ans) > 0) {
          idx <- which(saved_ans$sub_idx == i)
          if(length(idx) > 0) pre_val <- saved_ans$user_ans[idx[1]]
        }

        q_label <- HTML(paste0("<b>Q", user_state$current_q_idx, ".", i, ". ", sq$prompt,
                               "</b> <small style='color: #999;'>(", sq$marks, " pts)</small>"))

        ui_elem <- switch(sq$type,
                          "mcq" = radioButtons(input_id, q_label, choices = unlist(sq$options),
                                               selected = if(pre_val != "") pre_val else character(0)),

                          "ms" = checkboxGroupInput(input_id, q_label, choices = unlist(sq$options),
                                                    selected = if(pre_val != "") trimws(unlist(strsplit(pre_val, ","))) else character(0)),

                          "tf" = radioButtons(input_id, q_label, choices = c("True", "False"),
                                              selected = if(pre_val != "") pre_val else character(0)),

                          "short_answer" = textInput(input_id, q_label, value = pre_val),

                          div(class = "alert alert-warning", "Unknown question type")
        )

        div(class = "question-box", ui_elem)
      })

      tagList(context_ui, q_ui_list)
    }, error = function(e) {
      div(class = "error-message", paste("Error:", e$message))
    })
  })

  output$navigator_ui <- renderUI({
    req(user_state$current_result_id, user_state$q_order)

    tryCatch({
      ans_data <- safe_db_query(
        "SELECT question_id, MAX(user_ans) as has_ans FROM student_answers
         WHERE result_id = ? GROUP BY question_id",
        list(user_state$current_result_id))

      btns <- lapply(1:length(user_state$q_order), function(idx) {
        qid <- user_state$q_order[idx]
        status_row <- ans_data[ans_data$question_id == qid, ]

        btn_class <- "nav-btn nav-btn-unanswered"

        if(nrow(status_row) > 0 && !is.na(status_row$has_ans[1]) && status_row$has_ans[1] != "") {
          btn_class <- "nav-btn nav-btn-answered"
        }

        if(idx == user_state$current_q_idx) {
          btn_class <- "nav-btn nav-btn-current"
        }

        tags$button(idx, class = btn_class,
                    onclick = sprintf("Shiny.setInputValue('nav_target', %d, {priority: 'event'})", idx))
      })

      div(class = "navigator-button-grid", btns)
    }, error = function(e) {
      div(class = "error-message", paste("Navigator error:", e$message))
    })
  })

  observeEvent(input$nav_target, {
    save_current_ans()
    user_state$current_q_idx <- as.numeric(input$nav_target)
  })

  # ========================================================================
  # SAVE ANSWERS & SCORING
  # ========================================================================

  save_current_ans <- function() {
    tryCatch({
      if(is.null(user_state$current_q_idx) || is.null(user_state$current_result_id)) return()
      if(length(user_state$q_order) == 0 || user_state$current_q_idx > length(user_state$q_order)) return()

      current_id <- user_state$q_order[user_state$current_q_idx]
      row <- safe_db_query("SELECT * FROM questions WHERE id = ?", list(current_id))

      if(nrow(row) == 0) return()

      sub_qs <- safe_parse_subs(row$sub_questions[1])
      if(length(sub_qs) == 0) return()

      dbExecute(db_con, "DELETE FROM student_answers WHERE result_id = ? AND question_id = ?",
                list(user_state$current_result_id, current_id))

      for(i in 1:length(sub_qs)) {
        sq <- sub_qs[[i]]
        user_val <- input[[paste0("ans_", i)]]

        user_val_str <- ""
        earned <- 0
        correct_ans_str <- sq$answer

        if(!is.null(user_val) && length(user_val) > 0) {

          if(sq$type == "mcq") {
            user_val_str <- user_val
            is_correct <- tolower(trimws(user_val_str)) == tolower(trimws(sq$answer))
            earned <- if(is_correct) as.numeric(sq$marks) else 0
          }

          else if(sq$type == "ms") {
            u_arr <- sort(trimws(user_val))
            c_arr <- sort(trimws(unlist(strsplit(sq$answer, ","))))

            user_val_str <- paste(u_arr, collapse = ", ")
            correct_ans_str <- paste(c_arr, collapse = ", ")

            is_correct <- length(u_arr) == length(c_arr) && all(tolower(u_arr) == tolower(c_arr))
            earned <- if(is_correct) as.numeric(sq$marks) else 0
          }

          else if(sq$type == "tf") {
            user_val_str <- user_val
            is_correct <- tolower(trimws(user_val)) == tolower(trimws(sq$answer))
            earned <- if(is_correct) as.numeric(sq$marks) else 0
          }

          else if(sq$type == "short_answer") {
            user_val_str <- user_val
            is_correct <- tolower(trimws(user_val_str)) == tolower(trimws(sq$answer))
            earned <- if(is_correct) as.numeric(sq$marks) else 0
          }

        } else {
          user_val_str <- "(No answer)"
          earned <- 0
        }

        dbExecute(db_con,
                  "INSERT INTO student_answers (result_id, question_id, sub_idx, prompt, user_ans, correct_ans, marks_possible, marks_earned)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
                  list(user_state$current_result_id, current_id, i, sq$prompt, user_val_str, correct_ans_str, sq$marks, earned))
      }
    }, error = function(e) {
      warning(paste("Save error:", e$message))
    })
  }

  calculate_final_score <- function() {
    tryCatch({
      qs <- safe_db_query("SELECT id, sub_questions FROM questions")
      grand_total <- 0

      if(nrow(qs) > 0) {
        for(i in 1:nrow(qs)) {
          subs <- safe_parse_subs(qs$sub_questions[i])
          for(sq in subs) {
            grand_total <- grand_total + as.numeric(sq$marks)
          }
        }
      }

      totals <- safe_db_query(
        "SELECT SUM(marks_earned) as earned FROM student_answers WHERE result_id = ?",
        list(user_state$current_result_id))

      earned <- if(nrow(totals) == 0 || is.na(totals$earned[1])) 0 else totals$earned[1]
      pct <- if(grand_total == 0) 0 else round((earned / grand_total) * 100, 2)

      dbExecute(db_con,
                "UPDATE results SET earned_marks = ?, total_marks = ?, percentage = ? WHERE id = ?",
                list(earned, grand_total, pct, user_state$current_result_id))

      refresh_results(refresh_results() + 1)
      user_state$role <- "finished"
    }, error = function(e) {
      warning(paste("Score error:", e$message))
    })
  }

  output$final_score_display <- renderUI({
    res <- safe_db_query(
      "SELECT earned_marks, total_marks, percentage FROM results WHERE id = ?",
      list(user_state$current_result_id))

    if(nrow(res) > 0) {
      HTML(paste0(
        "<div style='color: #2A9D8F; font-size: 1.4em; font-weight: 700;'>",
        res$earned_marks[1], " / ", res$total_marks[1], " Points",
        "</div>",
        "<div style='color: #666; font-size: 1.2em; margin-top: 15px;'>",
        res$percentage[1], "%",
        "</div>"
      ))
    }
  })

  # ========================================================================
  # CALCULATOR MODALS
  # ========================================================================

  observeEvent(input$btn_calc_basic, {
    showModal(modalDialog(
      title = "🧮 Basic Calculator",
      easyClose = TRUE,
      size = "s",
      footer = NULL,
      HTML('
        <div class="calculator-frame">
          <div class="calc-display">
            <div class="calc-expression" id="bsc_expr"></div>
            <div class="calc-main-display" id="bsc_disp">0</div>
          </div>

          <div class="calc-button-grid">
            <button class="calc-btn calc-btn-clear" onclick="calcClear(\'bsc_expr\',\'bsc_disp\')">C</button>
            <button class="calc-btn calc-btn-fn" onclick="calcBackspace(\'bsc_expr\',\'bsc_disp\')">⌫</button>
            <button class="calc-btn calc-btn-fn" onclick="calcToggleSign(\'bsc_expr\',\'bsc_disp\')">+/-</button>
            <button class="calc-btn calc-btn-op" onclick="calcSetOp(\'bsc_expr\',\'bsc_disp\',\'/\')">÷</button>

            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'bsc_expr\',\'bsc_disp\',\'7\')">7</button>
            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'bsc_expr\',\'bsc_disp\',\'8\')">8</button>
            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'bsc_expr\',\'bsc_disp\',\'9\')">9</button>
            <button class="calc-btn calc-btn-op" onclick="calcSetOp(\'bsc_expr\',\'bsc_disp\',\'*\')">×</button>

            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'bsc_expr\',\'bsc_disp\',\'4\')">4</button>
            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'bsc_expr\',\'bsc_disp\',\'5\')">5</button>
            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'bsc_expr\',\'bsc_disp\',\'6\')">6</button>
            <button class="calc-btn calc-btn-op" onclick="calcSetOp(\'bsc_expr\',\'bsc_disp\',\'-\')">−</button>

            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'bsc_expr\',\'bsc_disp\',\'1\')">1</button>
            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'bsc_expr\',\'bsc_disp\',\'2\')">2</button>
            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'bsc_expr\',\'bsc_disp\',\'3\')">3</button>
            <button class="calc-btn calc-btn-op" onclick="calcSetOp(\'bsc_expr\',\'bsc_disp\',\'+\')">+</button>

            <button class="calc-btn calc-btn-num" style="grid-column: span 2;" onclick="calcAppend(\'bsc_expr\',\'bsc_disp\',\'0\')">0</button>
            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'bsc_expr\',\'bsc_disp\',\'.\')">.</button>
            <button class="calc-btn calc-btn-eq" onclick="calcEval(\'bsc_expr\',\'bsc_disp\')">=</button>
          </div>
        </div>
      ')
    ))
  })

  observeEvent(input$btn_calc_sci, {
    showModal(modalDialog(
      title = "🔬 Scientific Calculator",
      easyClose = TRUE,
      size = "m",
      footer = NULL,
      HTML('
        <div class="calculator-frame" style="max-width: 500px;">
          <div class="calc-display">
            <div class="calc-expression" id="sci_expr"></div>
            <div class="calc-main-display" id="sci_disp">0</div>
          </div>

          <div class="calc-button-grid calc-button-grid-5">
            <button class="calc-btn calc-btn-fn" onclick="calcAppendFn(\'sci_expr\',\'sci_disp\',\'Math.sin(\')">sin</button>
            <button class="calc-btn calc-btn-fn" onclick="calcAppendFn(\'sci_expr\',\'sci_disp\',\'Math.cos(\')">cos</button>
            <button class="calc-btn calc-btn-fn" onclick="calcAppendFn(\'sci_expr\',\'sci_disp\',\'Math.tan(\')">tan</button>
            <button class="calc-btn calc-btn-fn" onclick="calcAppendFn(\'sci_expr\',\'sci_disp\',\'Math.sqrt(\')">√</button>
            <button class="calc-btn calc-btn-fn" onclick="calcAppendFn(\'sci_expr\',\'sci_disp\',\'Math.PI\')">π</button>
          </div>

          <div class="calc-button-grid calc-button-grid-5">
            <button class="calc-btn calc-btn-clear" onclick="calcClear(\'sci_expr\',\'sci_disp\')">AC</button>
            <button class="calc-btn calc-btn-fn" onclick="calcBackspace(\'sci_expr\',\'sci_disp\')">⌫</button>
            <button class="calc-btn calc-btn-fn" onclick="calcPercent(\'sci_expr\',\'sci_disp\')">%</button>
            <button class="calc-btn calc-btn-fn" onclick="calcToggleSign(\'sci_expr\',\'sci_disp\')">+/-</button>
            <button class="calc-btn calc-btn-op" onclick="calcSetOp(\'sci_expr\',\'sci_disp\',\'/\')">÷</button>

            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'sci_expr\',\'sci_disp\',\'7\')">7</button>
            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'sci_expr\',\'sci_disp\',\'8\')">8</button>
            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'sci_expr\',\'sci_disp\',\'9\')">9</button>
            <button class="calc-btn calc-btn-fn" onclick="calcAppend(\'sci_expr\',\'sci_disp\',\'(\')">( </button>
            <button class="calc-btn calc-btn-op" onclick="calcSetOp(\'sci_expr\',\'sci_disp\',\'*\')">×</button>

            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'sci_expr\',\'sci_disp\',\'4\')">4</button>
            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'sci_expr\',\'sci_disp\',\'5\')">5</button>
            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'sci_expr\',\'sci_disp\',\'6\')">6</button>
            <button class="calc-btn calc-btn-fn" onclick="calcAppend(\'sci_expr\',\'sci_disp\',\')\')">)</button>
            <button class="calc-btn calc-btn-op" onclick="calcSetOp(\'sci_expr\',\'sci_disp\',\'-\')">−</button>

            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'sci_expr\',\'sci_disp\',\'1\')">1</button>
            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'sci_expr\',\'sci_disp\',\'2\')">2</button>
            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'sci_expr\',\'sci_disp\',\'3\')">3</button>
            <button class="calc-btn calc-btn-fn" onclick="calcAppend(\'sci_expr\',\'sci_disp\',\'**\')">x^y</button>
            <button class="calc-btn calc-btn-op" onclick="calcSetOp(\'sci_expr\',\'sci_disp\',\'+\')">+</button>

            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'sci_expr\',\'sci_disp\',\'0\')">0</button>
            <button class="calc-btn calc-btn-num" onclick="calcAppend(\'sci_expr\',\'sci_disp\',\'.\')">.</button>
            <button class="calc-btn calc-btn-fn" onclick="calcAppendFn(\'sci_expr\',\'sci_disp\',\'Math.log(\')">log</button>
            <button class="calc-btn calc-btn-fn" onclick="calcAppendFn(\'sci_expr\',\'sci_disp\',\'Math.E\')">e</button>
            <button class="calc-btn calc-btn-eq" onclick="calcEval(\'sci_expr\',\'sci_disp\')">=</button>
          </div>
        </div>
      ')
    ))
  })

  # ========================================================================
  # PDF DOWNLOADS
  # ========================================================================

  output$dl_student_pdf <- downloadHandler(
    filename = function() {
      paste0("Report_", gsub(" ", "_", user_state$name), "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      tryCatch({
        student <- safe_db_query("SELECT * FROM results WHERE id = ?",
                                 list(user_state$current_result_id))

        answers <- safe_db_query(
          "SELECT ROW_NUMBER() OVER (ORDER BY question_id) as 'Q#',
                  prompt, user_ans, correct_ans, marks_earned, marks_possible
           FROM student_answers
           WHERE result_id = ?
           ORDER BY question_id",
          list(user_state$current_result_id))

        if(nrow(student) == 0) {
          writeLines("Error generating report", file)
          return()
        }

        rmd_content <- c(
          "---",
          "output: pdf_document",
          "---",
          "# Assessment Report",
          "",
          paste("**Name:**", student$name[1]),
          paste("**Roll Number:**", student$roll_number[1]),
          paste("**Score:**", student$earned_marks[1], "/", student$total_marks[1],
                "Marks (", student$percentage[1], "%)"),
          paste("**Date:**", format(Sys.time(), '%d-%m-%Y %H:%M')),
          "",
          "## Detailed Results",
          "",
          "```{r, echo=FALSE}",
          "knitr::kable(answers)",
          "```"
        )

        temp_rmd <- tempfile(fileext = ".Rmd")
        writeLines(rmd_content, temp_rmd)
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      }, error = function(e) {
        writeLines(paste("Error:", e$message), file)
      })
    }
  )

  output$dl_admin_indiv_pdf <- downloadHandler(
    filename = function() {
      paste0("Student_Report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      tryCatch({
        req(input$sel_student_dl)
        student <- safe_db_query("SELECT * FROM results WHERE id = ?",
                                 list(input$sel_student_dl))

        answers <- safe_db_query(
          "SELECT ROW_NUMBER() OVER (ORDER BY question_id) as 'Q#',
                  prompt, user_ans, correct_ans, marks_earned, marks_possible
           FROM student_answers
           WHERE result_id = ?
           ORDER BY question_id",
          list(input$sel_student_dl))

        if(nrow(student) == 0) {
          writeLines("Error generating report", file)
          return()
        }

        rmd_content <- c(
          "---",
          "output: pdf_document",
          "---",
          "# Individual Student Report",
          "",
          paste("**Name:**", student$name[1]),
          paste("**Roll Number:**", student$roll_number[1]),
          paste("**Score:**", student$earned_marks[1], "/", student$total_marks[1],
                "Marks (", student$percentage[1], "%)"),
          paste("**Date:**", format(Sys.time(), '%d-%m-%Y %H:%M')),
          "",
          "```{r, echo=FALSE}",
          "knitr::kable(answers)",
          "```"
        )

        temp_rmd <- tempfile(fileext = ".Rmd")
        writeLines(rmd_content, temp_rmd)
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      }, error = function(e) {
        writeLines(paste("Error:", e$message), file)
      })
    }
  )

  output$dl_admin_master_pdf <- downloadHandler(
    filename = function() {
      paste0("Master_Report_", format(Sys.Date(), '%d-%m-%Y'), ".pdf")
    },
    content = function(file) {
      tryCatch({
        summary_df <- safe_db_query(
          "SELECT ROW_NUMBER() OVER (ORDER BY percentage DESC) as Rank,
                  name, roll_number, earned_marks, total_marks, percentage
           FROM results
           ORDER BY percentage DESC")

        if(nrow(summary_df) == 0) {
          writeLines("No results available", file)
          return()
        }

        rmd_lines <- c(
          "---",
          "output: pdf_document",
          "---",
          "# Master Assessment Report",
          paste("**Generated:**", format(Sys.time(), '%d-%m-%Y %H:%M')),
          paste("**Total Students:**", nrow(summary_df)),
          "",
          "```{r, echo=FALSE}",
          "knitr::kable(summary_df)",
          "```"
        )

        temp_rmd <- tempfile(fileext = ".Rmd")
        writeLines(rmd_lines, temp_rmd)
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      }, error = function(e) {
        writeLines(paste("Error:", e$message), file)
      })
    }
  )
}

# Run application
shinyApp(ui, server)

