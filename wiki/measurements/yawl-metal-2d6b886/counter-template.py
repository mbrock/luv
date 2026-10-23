import plistlib
import sys
source = '/Applications/Xcode.app/Contents/Applications/Instruments.app/Contents/Packages/GPU.instrdst/Contents/Templates/Metal System Trace.tracetemplate'
with open(source, 'rb') as f:
    template = plistlib.load(f)
objects = template['$objects']
profile = int(sys.argv[1]) if len(sys.argv) > 1 else 1
values = {'counterprofile': profile, 'counterprofileinternal': profile,
          'counterdeviceinternal': 4294968364, 'counterdevice': 4294968364,
          'shaderprofiler': True, 'shaderprofilerinternal': True}
for obj in list(objects):
    if isinstance(obj, dict) and 'NS.keys' in obj:
        for i, key in enumerate(obj['NS.keys']):
            name = objects[key.data]
            if isinstance(name, str) and name in values:
                objects.append(values[name])
                obj['NS.objects'][i] = plistlib.UID(len(objects) - 1)
with open('.amp/in/counters.tracetemplate', 'wb') as f:
    plistlib.dump(template, f, fmt=plistlib.FMT_BINARY)
