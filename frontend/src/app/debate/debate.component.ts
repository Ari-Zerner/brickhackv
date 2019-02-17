import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Debate } from '../models/debate';
import { UserService } from '../services/user.service';
import { PostOpinion } from '../models/post-opinion';
import { Opinion } from '../models/opinion';

@Component({
  selector: 'app-debate',
  templateUrl: './debate.component.html',
  styleUrls: ['./debate.component.scss']
})
export class DebateComponent implements OnInit {

	debate:Debate = null;
	usersOpinion:string;

	constructor(private userService:UserService, private route: ActivatedRoute,) { }

	ngOnInit() {
		const id = +this.route.snapshot.paramMap.get('id');
		this.userService.getDebate(id).subscribe((_)=>{
			this.debate = _;

			//dev purposes only
			// this.debate = new Debate();
			// this.debate.id = 1;
			// this.debate.title = "my debate";
			// this.debate.subtitle = "some brief description of the debate";
			// this.debate.description = "a realllyyyyy long description";
			// this.debate.opinionCount = 234;
			// this.debate.viewCount = 5231;
			// this.debate.myOpinion = null;
		});
	}

	postOpinion(){
		const postOpinion = new PostOpinion();
		postOpinion.debateId = this.debate.id;
		postOpinion.userId = this.userService.currentUser.id;
		postOpinion.opinion = this.usersOpinion;
		this.userService.opine(postOpinion).subscribe((_)=>{
			if(_){
				this.debate.myOpinion = new Opinion();
				this.debate.myOpinion.authorId = this.userService.currentUser.id;
				this.debate.myOpinion.description = this.usersOpinion;
			}else{
				// TODO message: Posting opinion failed
				console.log("Posting opinion failed");
			}
		});
	}

}
