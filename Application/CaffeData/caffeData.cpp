#include "caffeData.h"
#include <string>
#include "boost/scoped_ptr.hpp"
#include "glog/logging.h"
#include "google/protobuf/text_format.h"
#include "stdint.h"

#include "caffe/proto/caffe.pb.h"
#include "caffe/util/db.hpp"
#include "caffe/util/format.hpp"
#include <sstream>

using caffe::Datum;
using boost::scoped_ptr;
using std::string;
namespace db = caffe::db;

static class db::DB** trainDBList = NULL;
static int numDB = 0;

std::string NumberToString ( int Number )
{

    std::ostringstream ss;

    ss << Number;

    return ss.str();

}


void openDatabase(char * dbType, char * path, int num)
{
        std::string typeString(dbType);
        numDB = num;
        
        trainDBList = new db::DB*[num];
        for (int i = 0; i < num ; i++){
            std::string pathString(path);
            pathString += "_";
            trainDBList[i] = db::GetDB(typeString);
            trainDBList[i]->Open(pathString.append(NumberToString(i)),db::NEW);            
        }
}

void closeDatabase()
{
    for (int i = 0; i < numDB; i++){
	trainDBList[i]->Close();
	free(trainDBList[i]);
    }
    free(trainDBList);
}

void saveData(int dbIdx, int width, int height, int channel,int batch, int offset, unsigned char ** data, int *label)
{
	scoped_ptr<db::Transaction> txn(trainDBList[dbIdx]->NewTransaction());
	int len = channel * height * width;
	Datum datum;
	datum.set_channels(channel);
	datum.set_height(height);
	datum.set_width(width);

	for (int i = 0; i < batch; i++)
	{
		datum.set_label(label[i]);
                datum.set_data(data[i], len);
		string out;
		datum.SerializeToString(&out);
		txn->Put(caffe::format_int(i+offset, 5), out);
	}
	txn->Commit();
}


// int main(int argc, char** argv) {
// 	return 0;
// }
