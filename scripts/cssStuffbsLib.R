my_theme <- bs_theme(version = 5, preset = "flatly") %>%
  bs_add_rules(as_sass("
    /* Center everything in the navbar container */
.navbar .container-fluid {
    display: flex !important;
    align-items: center;
}

/* Targets both the Brand and the Menu labels */
.navbar-brand,
.navbar-nav > li > a {
  display: flex !important;
  align-items: center;
  height: 80px;      /* Your custom height */
  font-size: 18px;   /* Your custom size */
}

/* Dropdown Arrow: Fixes the 'floating' caret */
.navbar-nav .dropdown .caret {
  margin-left: 5px;
  display: inline-block;
  vertical-align: middle;
}

/* Mobile Toggle: Alignment for small screens */
.navbar-toggle {
    margin-top: 4px;
    margin-bottom: 4px;
}    

/* for render report button not allowing click on disable */

.btn[disabled], .btn.disabled {
    pointer-events: none !important;  /* Physically prevents the click from registering */
    cursor: not-allowed !important;   /* Forces the red 'stop' symbol */
    opacity: 0.5 !important;          /* Makes it look visually 'off' */
    filter: grayscale(100%);          /* Optional: removes CPW green/blue when disabled */
}

/* CPW Theme beginning: not suepr essential */

.navbar-default {
  background-color: #245d38 !important; /* CPW Green */
    border-color: #1e4d2e !important;
}

/* The text/labels in the navbar */
  .navbar-default .navbar-nav > li > a, 
.navbar-default .navbar-brand {
  color: #FFFFFF !important; /* White text */
}

/* Hover and Active states */
  .navbar-default .navbar-nav > li > a:hover,
.navbar-default .navbar-nav > .open > a:hover {
  background-color: #1e4d2e !important; /* Slightly darker green on hover */
    color: #ffd100 !important; /* CPW Yellow on hover */
}

/* Fix the dropdown arrow color to match white text */
  .navbar-default .navbar-nav .dropdown .caret {
    border-top-color: #FFFFFF !important;
      border-bottom-color: #FFFFFF !important;
  }

.dropdown-menu {

  background-color: #245d38 !important;
    border: 1px solid #1e4d2e;
}

/* Dropdown text color */
  .dropdown-menu > li > a {
    
    color: #FFFFFF !important;
      padding: 10px 20px;
  }

/* Hover effect inside the dropdown */
  .dropdown-menu > li > a:hover {
    background-color: #1e4d2e !important;
      color: #ffd100 !important; /* CPW Yellow */
  }

.navbar-default .navbar-nav > .dropdown > a,
.navbar-default .navbar-nav > .dropdown > a:focus,
.navbar-default .navbar-nav > .active > a {
  background-color: #245d38 !important; /* Matches your main CPW Green */
    color: #FFFFFF !important;
}

/* If you want it to stay the darker green while open (to show it's active) */
.navbar-default .navbar-nav > .open > a {
    background-color: #1e4d2e !important; 
}

.navbar-nav .nav-item.dropdown .nav-link.dropdown-toggle {
    /* 1. Add rounding */
    border-radius: 20px !important; 
    
    /* 2. Buffer: Add horizontal space so the rounding isn't cramped */
    padding-left: 15px !important;
    padding-right: 15px !important;

    /* Optional: Transition for smoothness */
    transition: all 0.1s ease;
    
}

.navbar-nav .nav-item.dropdown .nav-link.dropdown-toggle.active {
    color: #ffd100 !important;          /* Yellow text */
    background-color: transparent;       /* Keep background clean or set to your green */
    border-bottom: 2px solid #ffd100 !important; /* Replaces white line with yellow */
    background-color: #1e4d2e !important; 
}

.dropdown-menu > .active > a, 
.dropdown-menu > .active > a:hover, 
.dropdown-menu > .active > a:focus {

    background-color: #1e4d2e !important; /* Darker CPW Green */
    color: #ffd100 !important;            /* CPW Yellow text for the active item */
}

.navbar {
    border-bottom: 3px solid #ffd100 !important; /* 3px thick, solid CPW Yellow */
}




.btn-cpw-sidebar {
    background-color: #245d38 !important; /* CPW Green */
    color: #FFFFFF !important;
    border-color: #1e4d2e !important;
    font-weight: bold;
}

/* Define the hover state for that custom class */
.btn-cpw-sidebar:hover {
    background-color: #1e4d2e !important;
    color: #ffd100 !important; /* Yellow text on hover */
    border-color: #ffd100 !important;
}

/* 1. Sidebar Background */
.well {
    background-color: #f1f8f3 !important; /* Very light green wash */
    border: 1px solid #245d38 !important;  /* CPW Green border */
    border-radius: 10px;
}

/* 2. Slider - The Bar (the background of the slider) */
.irs-bar, .irs-bar-edge {
    background-color: #245d38 !important; /* CPW Green for the 'filled' part */
    border-top: 1px solid #245d38 !important;
    border-bottom: 1px solid #245d38 !important;
}

.irs-line {
    background: #e2f0d9 !important; /* Light green for the 'unfilled' part */
    border: 1px solid #cbdcc0 !important;
}

/* 3. Slider - The Handle (the circle you grab) */
.irs-single, .irs-bar-edge, .irs-from, .irs-to {
    background: #245d38 !important; /* Tooltip backgrounds */
}

.irs-handle {
    background-color: #ffd100 !important; /* CPW Yellow handle to make it pop */
    border: 1px solid #245d38 !important;
    border-radius: 50% !important; /* Makes the handle a perfect circle */
}

/* 4. Slider - Text Labels (The numbers) */
.irs-grid-text {
    color: #245d38 !important;
    font-weight: bold;
}

/* --- Active Tab (The one currently selected) --- */
.nav-tabs .nav-link.active, 
.nav-tabs .nav-item.show .nav-link {
    background-color: #e2f0d9 !important; /* Light Green Background */
    color: #245d38 !important;            /* CPW Green Text */
    border: 1px solid #245d38 !important; /* CPW Green Border */
    border-bottom-color: #e2f0d9 !important; /* Blends the bottom into the panel */
    border-radius: 4px 4px 0 0;           
    font-weight: bold;
}

/* --- Inactive Tabs (The ones waiting to be clicked) --- */
.nav-tabs .nav-link {
    color: #245d38 !important;            /* CPW Green text */
    background-color: #f1f8f3 !important; /* Very light green background */
    border: 1px solid #dee2e6 !important; /* Light grey default border */
    margin-right: 2px;
    transition: all 0.1s ease-in-out;
}
/* --- Hover State nav-link class */
.nav-link:hover {
    color: #ffd100 !important;
}
/* --- Hover State for Inactive Tabs --- */
.nav-tabs .nav-link:hover:not(.active) {
    background-color: #e2f0d9 !important; /* Light green wash */
    color: #1e4d2e !important;            /* Darker green text */
    border-color: #245d38 !important;
}

/* --- Remove the default Bootstrap 'blue' glow/outline on click --- */
.nav-tabs .nav-link:focus {
    box-shadow: none !important;
}



/* Background of the actual dropdown list (the 'dropbox') */
.vscomp-dropbox-container {
    background-color: #FFFFFF !important;
    border: 1px solid #245d38 !important;
}



/* Active/Hover state inside the dropdown */
.vscomp-option.active, .vscomp-option.focused {
    background-color: #245d38 !important; /* CPW Green */
    color: #FFFFFF !important;            /* White text */
}

input[type='radio'] {
    accent-color: #245d38 !important; /* Forces the internal check/box to CPW Green */
    cursor: pointer;
    transform: scale(1.1); 
}

.green-row {
          background-color: #f1f8f3; /* A very light green that complements the green skin */
          padding-top: 15px;
          margin-bottom: 20px;
          border-radius: 5px;
          border-color: #245d38;
        }

.control-label {
  font-weight: bold !important;
}

.normal-label-row .control-label, 
.normal-label-row label {
  font-weight: normal !important;
}

/* DT table pagination buttons */
.pagination .page-item.active .page-link {
    background-color: #0f7864 !important;
    border-color: #0f7864 !important;
    color: white !important; /* Ensure text remains readable */
}

/* 2. Target the OTHER (unselected) pagination buttons */
.pagination .page-item .page-link {
    background-color: #d1e7e3 !important; /* A lighter shade of your green */
    border-color: #badbd5 !important;
    color: #0f7864 !important; /* Keep text the darker green for consistency */
}

/* 3. Add a hover effect for better user experience */
.pagination .page-item .page-link:hover {
    background-color: #aed1cb !important;
    border-color: #0f7864 !important;
    color: #0f7864 !important;
}


  "))