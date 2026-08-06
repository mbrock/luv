;;;; make-vk

(in-package :vk-generator)

(defun generate (&key
                   (version (car (last *versions*)))
                   (package-dir (first ql:*local-project-directories*) package-dir-supplied-p)
                   (vk-xml-dir (uiop:temporary-directory))
                   (force-download nil)
                   (dry-run nil))
  (declare (type string version))
  (declare (type pathname package-dir))
  (declare (type pathname vk-xml-dir))
  "Generates the package VK from an xml file.

  :VERSION        --- The version tag of the Vulkan version for which the VK package is generated.
                      See *VERSIONS* for known versions and the Vulkan-Docs repository for a list of exising version tags: https://github.com/KhronosGroup/Vulkan-Docs/releases
                      Default: (CAR (LAST VK-GENERATOR:*VERSIONS*))
  :PACKAGE-DIR    --- The directory where the VK package is created.
                      The sources of the created package will lie in <:PACKAGE-DIR>/vk.
                      Default: (FIRST QL:*LOCAL-PROJECT-DIRECTORIES*)
  :VK-XML-DIR     --- The directory to which the vk.xml is downloaded from the Vulkan-Docs GitHub repository.
                      If vk-<:VERSION>.xml exists at VK-XML-DIR and :FORCE-DOWNLOAD is NIL, the download of the vk.xml file is omitted.
                      Default: UIOP:TEMPORARY-DIRECTORY
  :FORCE-DOWNLOAD --- If this is truthy the vk.xml file will be downloaded even if it already exists in the file system.
                      Default: NIL
  :DRY-RUN        --- If this is truthy, the generated code will be printed in the REPL rather than written to the file system.
                      Default: NIL

See *VERSIONS*
See ENSURE-VK-XML
See PARSE-VK-XML
See WRITE-VK-PACKAGE
"
  (unless (find version *versions* :test #'string=)
    (warn "Unknown version: ~s, trying anyway" version))
  (when (and package-dir-supplied-p
             (not (find package-dir ql:*local-project-directories*)))
    (warn ":PACKAGE-DIR is not included in QL:*LOCAL-PROJECT-DIRECTORIES*: ~s" package-dir))
  (let ((vk-dir (merge-pathnames (make-pathname :directory '(:relative "vk")) package-dir)))
    (ensure-directories-exist vk-dir :verbose t)
    (write-vk-package
     (parse-vk-xml
      version
      (ensure-vk-xml version :vk-xml-dir vk-xml-dir :force-download force-download))
     vk-dir
     dry-run)))

