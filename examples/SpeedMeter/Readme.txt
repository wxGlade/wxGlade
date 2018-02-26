This (Python only) example shows how to use the Custom Widget to include the
SpeedMeter widget from wx.lib.agw.speedmeter:

 - the property "Class" is set to "SM.SpeedMeter"
 - on the "Widget" page, the "Arguments" are defined to include the style
   argument
 - on the "Code" page:
   - the speedmeter module is imported as "SM"
   - pi and sqrt are imported, as they are required for setting properties
   - some comments are added
   - several properties are set in "Code to be inserted after"
   - several properties are defined in "Extra properties..."
     e.g. "AngleRange:-pi/6, 7*pi/6"
     will be translated to speed_meter.SetAngleRange(-pi/6, 7*pi/6)


The slider that is included, has a lambda function as event handler to set the
speed meter when you drag the slider.
