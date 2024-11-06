"
This module provides template functions and macros for handling and applying templates.

The module includes the following main functions and macros:

1. find_template: locate template files in the 'templates' directory.
2. complete_template: load and apply templates with given parameters.
4. deftemplate: define functions for applying specific templates.

"

(require hyrule.argmove [-> ->>])

(import hyjinx.lib [slurp filenames])

(import tomllib)
(import pathlib [Path])


(defclass TemplateError [RuntimeError])

;; * file and toml utilities
;; -----------------------------------------------------------------------------

(defn file-exists [path]
  "Return Path object if it exists as a file, otherwise None."
  (when (.exists path)
    path))

(defn find-toml-file [#^ str name]
  "Locate a toml file.
  It will look under, in order:
    - `$pwd/templates/`         -- templates in the current dir
    - `$XDG_CONFIG_DIR/trag/`   -- user-defined config templates
    - `$module_dir/templates/`  -- the standard templates
  "
  (let [fname (+ name ".toml")]
    (or
      (file-exists (Path "templates" fname))
      (file-exists (Path (user-config-dir __package__) fname))
      (file-exists (Path (os.path.dirname __file__) "templates" fname))
      (raise (FileNotFoundError fname)))))

(defn load-template [#^ Path fname #* keys]
  "Get values in toml file `fname` like a hashmap, but default to None."
  (try
    (-> fname
        (slurp)
        (tomllib.loads)
        (get #* keys))
    (except [KeyError]
      None)))
  
(defn find-templates []
  "All standard template toml files.
  Returns a list of template names (not paths)"
  (lfor template-file (filenames (. (Path __file__) parent) "templates")
    :if (.endswith template-file ".toml")
    (. (Path template-file) stem)))

(defn find-template [#^ str name]
  "Returns the Path of the template name."
  (let [fname (+ name ".toml")]
    (Path (. (Path __file__) parent) "templates" fname)))

;; * template completion
;; -----------------------------------------------------------------------------

(defn complete-template [#^ str template-file #^ str template-name #** kwargs]
  "Locate a template toml file under `$module_dir/templates`.
  Load the template with name `template-file.toml` from the templates directory
  and apply python string `.format` method.
  Replace each `{kwarg}` with its value to form the one-shot user prompt."
  (let [template (load-template (find-template template-file) template-name)]
    (if template
      (.format template #** kwargs)
      (raise (TemplateError f"Template '{template-name}' not found in file '{template-file}.toml'.")))))

(defmacro deftemplate [template-file]
  "Macro that defines a function named after `template-file`.
  For example, `(deftemplate \"context\")` creates a function
  `context` that when called like `(context \"world\" #** kwargs)`,
  applies the `world` template defined in `context.toml`."
  (let [docstr (.format "Applies the kwargs to one of the `{templates}`\ntemplates defined in `{template_file}.toml`, specified by the `template-name` arg."
                        :template-file template-file
                        :templates (.join "`, `" (.keys (tomllib.loads (slurp (find-template template-file))))))
        name (cond
               (isinstance template-file hy.models.Symbol) (str template-file)
               (isinstance template-file str) template-file
               (isinstance template-file Path) template-file.name
               ;; might be a symbol referring to a function
               :else template-file.__name__)]
    `(defn ~template-file [template-name #** kwargs]
       ~docstr
       (import trag.template [complete-template])
       (complete-template ~name template-name #** kwargs))))

