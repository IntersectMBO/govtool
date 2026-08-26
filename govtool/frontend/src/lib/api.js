--- a/govtool/frontend/src/lib/api.js
+++ b/govtool/frontend/src/lib/api.js
@@ -15,7 +15,7 @@
-  key: '5d22e910',
+  key: import.meta.env.VITE_MOCKAROO_API_KEY ?? '',
