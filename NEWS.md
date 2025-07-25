# BRIDGE21 0.4.0

- Add example data set and list of variables included in the report
- Add an axial and sagittal image to be used when generating a sample report
- Create a new function called `save_xnat_images` that moves XNAT API code outside of the `generate_report` function
- Adjust the `generate_report` function to produce a sample report with `example_report` set to TRUE
- Add reports argument to allow manual control over the reports produced.

# BRIDGE21 0.3.0

- Add visual for body fat percentage from DXA scan
- Make DXA table and text conditional for bone mineral density
- Update visual for body mass index by removing history table
- Add weight history to DXA scan table
- Add BRIDGE21 hex sticker

# BRIDGE21 0.2.0

- Update formatting using Posit Air
- Changed NTG-EDSD to show domain scores
- Added modified cued recall test to report
- Change percentiles to overall and domain scores for DSMSE
- Updated MRI image search to work with DS-Cohort and ABC-DS studies on XNAT

# BRIDGE21 0.1.0

- Initial working version being distributed to participants.
- Includes visuals for blood pressure, physical activity, and body mass index
- Percentiles for Down Syndrome Mental State Exam (DSMSE)
- Overall Score for the NTG-EDSD
- Missing sections include APOE4, VO2 Peak, and Nutrition
