;; Code Author: Daniel Nass
;;

(defun c:G ( / sset idx ename obj vol matChoice density unitChoice weight pt txtObj textStr suffix cDoc space matName)
  (vl-load-com)
  
  ;; 1. Prompt user for material code selection
  (initget "AISI304 AISI316 ASTMA36 6061 1045 C26000 PVCMARROMKRN PVCBRANCO Custom")
  (setq matChoice (getkword "\nChoose Material [AISI304/AISI316/ASTMA36/6061/1045/C26000/PVCMARROMKRN/PVCBRANCO/Custom] <Custom>: "))
  
  ;; Default to Custom if user just presses Enter
  (if (not matChoice) (setq matChoice "Custom"))
  
  ;; 2. Assign density based on choice, or prompt if "Custom"
  (cond
    ((= matChoice "AISI304")  (setq density 7.93  matName "AISI 304"))
    ((= matChoice "AISI316")  (setq density 7.93  matName "AISI 316"))
    ((= matChoice "ASTMA36")  (setq density 7.85  matName "ASTM A36"))
    ((= matChoice "1045")     (setq density 7.85  matName "1045 Steel"))
    ((= matChoice "6061")     (setq density 2.70  matName "6061 Alum"))
    ((= matChoice "C26000")   (setq density 8.53  matName "C26000 Brass"))
    ((= matChoice "PVCMARROMKRN")   (setq density 1.37  matName "PVC Marrom Krona"))
    ((= matChoice "PVCBRANCO")   (setq density 1.5  matName "PVC Branco"))
    ((= matChoice "Custom")
     (setq density (getreal "\nEnter custom material density in g/cm³: "))
     (setq matName "Custom"))
  )
  
  ;; Proceed only if we have a valid density
  (if density
    (progn
      ;; 3. Ask user for the drawing unit context
      (initget "Millimeters Inches")
      (setq unitChoice (getkword "\nAre drawing units in [Millimeters/Inches] <Millimeters>: "))
      
      (if (not unitChoice) (setq unitChoice "Millimeters"))
      
      (princ (strcat "\nSelect 3D Solids to calculate weight (" matName "): "))
      
      ;; 4. Prompt user to select 3D solids
      (if (setq sset (ssget '((0 . "3DSOLID"))))
        (progn
          (setq cDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
          (setq space (if (= (getvar "CVPORT") 1)
                        (vla-get-PaperSpace cDoc)
                        (vla-get-ModelSpace cDoc)))
          
          (setq idx 0)
          ;; Loop through all selected solids
          (while (< idx (sslength sset))
            (setq ename (ssname sset idx))
            (setq obj (vlax-ename->vla-object ename))
            
            ;; Get the raw volume from AutoCAD
            (setq vol (vla-get-Volume obj))
            
            ;; 5. Apply unit conversion math based on unit choice
            (if (= unitChoice "Inches")
              (progn
                ;; Volume (in³) * Density (g/cm³) * Conversion Factor -> kg
                (setq weight (* vol density 0.016387))
                (setq suffix " kg")
              )
              (progn
                ;; Vol (mm³) * Density (g/cm³) / 1,000,000 -> kg
                (setq weight (/ (* vol density) 1000000.0))
                (setq suffix " kg")
              )
            )
            
            ;; Format the text string (includes material name for clarity)
            ;; Example: "Peso: 15.42 kg"
            (setq textStr (strcat "Peso: " (rtos weight 2 2) suffix))
            
            ;; Ask user where to place the text
            (setq pt (getpoint (strcat "\nPick insertion point for " matName " weight text: ")))
            
            (if pt
              (progn
                ;; Create MText at the picked point
                (setq txtObj (vla-addMText space (vlax-3d-point pt) 0.0 textStr))
                (vla-put-AttachmentPoint txtObj 1) ;; Top-Left alignment

		;; Sets height to 66
		(vla-put-Height txtObj 66.0)
              )
            )
            (setq idx (1+ idx))
          )
        )
        (princ "\nNo 3D Solids selected.")
      )
    )
    (princ "\nInvalid density input.")
  )
  (princ)
)

(princ "\nType 'G' to run the routine.")
(princ)
