import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { OpinionVotingComponent } from './opinion-voting.component';

describe('OpinionVotingComponent', () => {
  let component: OpinionVotingComponent;
  let fixture: ComponentFixture<OpinionVotingComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ OpinionVotingComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(OpinionVotingComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
