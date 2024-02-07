css = "
body { 
  background-color: #000000;
  color: #808080;
}

a, a:visited {
  color: #3b76ff
}

a:hover{
  color: #3bf2ff
}

/* Style for DT */
.dataTables_wrapper {
  background-color: #262626; 
  color: #999999;
}
table.dataTable tbody tr:nth-child(odd) {
  background-color: #1a1a1a;
}
table.dataTable tbody tr:nth-child(even) {
  background-color: #0d0d0d;
}


/* Style for wellPanel */
.well,
.modal-content {
  color: #cccccc;
  background-color: #1a1a1a;
  border-color: #808080;
}

/* Style for Input Box */
.form-control,
shiny-input-number form-control shiny-bound-input {
  color: #ffffff
  background-color: #000000;
}
/* Style for Input Box Arrows */
input[type='number']::-webkit-inner-spin-button,
input[type='number']::-webkit-outer-spin-button {
    -webkit-appearance: none; /* Removes default webkit appearance */
    margin: 0; /* Removes default margin */
}
input[type='number'] {
    -moz-appearance: textfield; /* Removes spin buttons in Firefox */
}


/* Style for Tabs */
.nav-tabs > li > a {
  background-color: #333333;
  color: #3b76ff; 
  border-color: #808080;
}
.nav-tabs > li > a:hover {
  background-color: #333333;
  color: #3bf2ff;
  border-color: #808080;
}
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover {
  background-color: #262626 !important; 
  color: #e6e6e6 !important;
  border-color: #808080 !important;
}

.btn {
  background-color: #262626 !important;
  color: #3b76ff !important;
  border-color: #808080 !important;
}
.btn:hover {

  color: #3bf2ff !important;
}

.checkbox input[type='checkbox'] {
  background-color: #000000;
  border-color: #808080;
}

.modal-dialog {
  width: 80%;
  max-width: 900px;
}

"