# KDD2015
![alt tag](http://dtcomp.g-i.asia/files/upload/cms/1/fe57d2863bd42bea9a8f31243fac8f0f.png)
<br>
### Backgrounds
High dropout rate in Massive Open Online Courses (MOOCs) is worrisome. To cope with the dropping out issue, students who are inclined to quit should be identified and efforts should be made to prevent the churn. Early prediction of an online course dropout is a fundamental problem for MOOC platforms. This competition will be dealing with dropout prediction in the XuetangX.com platform.
<br>

### Descriptions
This competition challenges participants to predict whether or not a user will drop a course in next 10 days based on his or her prior actions. We say a user U drops a course C  in the next 10 days if U  leaves no records of course C  in the log during the next 10 days. For more details about log, please refer to Datasets Section.

### Tips
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