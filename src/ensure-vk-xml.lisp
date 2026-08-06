;;;; ensure-vk-xml.lisp

(in-package :vk-generator)

(defun make-vk-xml-name (version)
  (declare (type string version))
  "Creates the name of a vk.xml file (without file type) from a version tag.

E.g. \"v1.2.153\" produces \"vk-v1.2.153\"

See *VERSIONS*
"
  (format nil "vk-~a" version))

(defun make-xml-pathname (version xml-dir)
  (declare (type string version))
  (declare (type pathname xml-dir))
    "Creates a pathname for a vk.xml file from a version tag.

E.g. #P\"/tmp/\" produces #P\"/tmp/vk-v1.2.153.xml\"

See *VERSIONS*
See MAKE-VK-XML-NAME
"
  (merge-pathnames (make-pathname :name (make-vk-xml-name version) :type "xml")
                   xml-dir))

(defun download-vk-xml (version out-dir)
  (declare (type string version))
  (declare (type pathname out-dir))
  "Downloads a vk.xml file for a given version tag from the Vulkan-Docs GitHub repository to a given directory.

See *VERSIONS*
See MAKE-XML-PATHNAME
See GET-XML-PATH
See https://github.com/KhronosGroup/Vulkan-Docs
"
  (trivial-download:download
   (format nil "https://raw.githubusercontent.com/KhronosGroup/Vulkan-Docs/~a/~avk.xml"
           version
           (namestring (get-xml-path version)))
   (make-xml-pathname version out-dir)))

(defun ensure-vk-xml (version &key (vk-xml-dir (uiop:temporary-directory)) (force-download nil))
  (declare (type string version))
  (declare (type pathname vk-xml-dir))
  "Ensures a vk.xml file for a given version tag exists in a given directory.

If the xml file for the given version tag does not exists in the given directory it is downloaded from the Vulkan-Docs GitHub repository.

If FORCE-DOWNLOAD is truthy a vk.xml file is downloaded whether it already exists in the given directory or not.

See *VERSIONS*
See MAKE-XML-PATHNAME
See DOWNLOAD-VK-XML
See https://github.com/KhronosGroup/Vulkan-Docs
"
  (let ((xml-pathname (make-xml-pathname version vk-xml-dir)))
    (ensure-directories-exist vk-xml-dir :verbose t)
    (when (or force-download
              (not (probe-file xml-pathname)))
      (format t "Download vk.xml~%")
      (download-vk-xml version vk-xml-dir))
    (format t "vk.xml is located at: ~s~%" xml-pathname)
    xml-pathname))
