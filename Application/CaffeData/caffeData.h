#ifndef _CAFFEDATA_H
#define _CAFFEDATA_H

#ifdef __cplusplus
extern "C" {
#endif

void openDatabase(char * dbType, char * path, int num);
void closeDatabase();
void saveData(int dbIdx, int width, int height, int channel,int batch, int offset, float ** data, int *label);

#ifdef __cplusplus
}
#endif

#endif /* _CAFFEDATA_H */

