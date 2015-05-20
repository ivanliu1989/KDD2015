# KDD2015
![alt tag](http://dtcomp.g-i.asia/files/upload/cms/1/fe57d2863bd42bea9a8f31243fac8f0f.png)
<br>
### Backgrounds:
High dropout rate in Massive Open Online Courses (MOOCs) is worrisome. To cope with the dropping out issue, students who are inclined to quit should be identified and efforts should be made to prevent the churn. Early prediction of an online course dropout is a fundamental problem for MOOC platforms. This competition will be dealing with dropout prediction in the XuetangX.com platform.
<br>

### Descriptions:
This competition challenges participants to predict whether or not a user will drop a course in next 10 days based on his or her prior actions. We say a user U drops a course C  in the next 10 days if U  leaves no records of course C  in the log during the next 10 days. For more details about log, please refer to Datasets Section.

### Tips:
1. Object
	- Course_id, module_id, category, children
	- Children #, Children category #, Children level
2. Enrolment
	- enrollment_id, username, course_id
3. Log
	- enrollment_id, username, course_id, time, source, event, object
	- time transformation (week, day, time)
	- time * event (#)
	- time * object (#)
	- time * source (#)
	- same event (problem) (#/%)
4. Truth
	- enrollment_id, target_value

### Features Generated:
1. Raw: (categorical)
	- username
	- course_id
2. Source: (numeric)
	- ServerCount
	- browserCount
3. Event: (numeric)
	- navigateCount
	- accessCount
	- problemCount
	- page_closeCount
	- videoCount
	- discussionCount
	- wikiCount
4. Time: (numeric)
	- sdTime
	- enrolmentTime / duration
	- hourMean / hourDistribution
	- skewnessTime
	- kurtosisTime
	- freqDist / FreqMean

### Feature Engineers:
1. Interaction
2. Centering & Scaling
3. Log transformation
4. One-hot encoding
5. PCA

### Models:
1. xgboost
2. vw
3. svm
4. nnets
5. rf
6. fm

### Steps:
1. Feature generation
2. Feature engineering
3. Feature importance
4. Training
5. Blending
